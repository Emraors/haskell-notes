module RecursionSchemes () where


import Data.Map as M
import Data.Set as S
import Data.Bool (bool)


-- | Fixpoint of a functor
newtype Fix f = Fix { unFix :: f (Fix f) }

-- | Catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix



data ListF a r = C a r | Nil deriving (Show, Eq)

instance Functor (ListF a) where
  fmap f (C a r) = C a (f r)
  fmap _ Nil       = Nil

type List a = Fix (ListF a)


data NatF r = Zero | Succ r deriving (Show, Eq)

instance Functor NatF where
  fmap _ Zero     = Zero
  fmap f (Succ r) = Succ (f r)

type Nat = Fix NatF

data ExprF r = Const Int
              | Var String
              | Add r r
              | Mul r r
              | IfNeg r r r
              deriving (Show, Eq, Ord)

instance Functor ExprF where
  fmap _ (Const i) = Const i
  fmap _ (Var s)   = Var s
  fmap f (Add r1 r2) = Add (f r1) (f r2)
  fmap f (Mul r1 r2) = Mul (f r1) (f r2)
  fmap f (IfNeg r1 r2 r3) = IfNeg (f r1) (f r2) (f r3)


type Expr = Fix ExprF

const' :: Int -> Expr
const' = Fix . Const

var :: String -> Expr
var = Fix . Var

add :: Expr -> Expr -> Expr
add e1 e2 = Fix (Add e1 e2)

mul :: Expr -> Expr -> Expr
mul e1 e2 = Fix (Mul e1 e2)

ifNeg :: Expr -> Expr -> Expr -> Expr
ifNeg e1 e2 e3 = Fix (IfNeg e1 e2 e3)


type Env = Map String Int

eval :: Env -> Expr -> Maybe Int
eval env = cata alg
  where
    alg :: ExprF (Maybe Int) -> Maybe Int
    alg (Const i) = Just i
    alg (Var s)   = M.lookup s env
    alg (Add r1 r2) = (+) <$> r1 <*> r2
    alg (Mul r1 r2) = (*) <$> r1 <*> r2
    alg (IfNeg t x y) = t >>= bool x y . (< 0)


pretty :: Expr -> String
pretty = cata alg
  where
    alg :: ExprF String -> String
    alg (Const i) = show i
    alg (Var s)   = s
    alg (Add r1 r2) = r1 ++ " + " ++ r2
    alg (Mul r1 r2) = r1 ++ " * " ++ r2
    alg (IfNeg t x y) = "if " ++ t ++ " < 0 then " ++ x ++ " else " ++ y

freeVars :: Expr -> Set String
freeVars = cata alg
  where
    alg :: ExprF (Set String) -> Set String
    alg (Const _) = S.empty
    alg (Var s)   = S.singleton s
    alg (Add r1 r2) = r1 `S.union` r2
    alg (Mul r1 r2) = r1 `S.union` r2
    alg (IfNeg t x y) = t `S.union` x `S.union` y

substitute :: Map String Expr -> Expr -> Expr
substitute env = cata alg
  where
    alg :: ExprF Expr -> Expr
    alg (Const i) = const' i
    alg (Var s)   = M.findWithDefault (var s) s env
    alg (Add r1 r2) = add r1 r2
    alg (Mul r1 r2) = mul r1 r2
    alg (IfNeg t x y) = ifNeg t x y




sub :: Expr -> Expr
sub = substitute (M.fromList [("x", var "y"), ("y", const' 2)])

expr :: Expr
expr = mul (add (const' 1) (var "x")) (ifNeg (var "y") (const' 2) (const' 3))

envEx :: Env
envEx = M.fromList [("x", 2), ("y", -1)]

-- | In haskell there is no difference between data and codata
newtype Cofix f = Cofix { unCofix :: f (Cofix f) }

-- | Anamorphism
ana' :: Functor f => (a -> f a) -> a -> Cofix f
ana' f = Cofix . fmap (ana' f) . f

ana :: Functor f => (a -> f a) -> a -> Fix f
ana f = Fix . fmap (ana f) . f


data StreamF a r = S a r deriving (Show, Eq)

instance Functor (StreamF a) where
  fmap f (S a r) = S a (f r)


type Stream a = Cofix (StreamF a)

conS :: a -> Stream a -> Stream a
conS a = Cofix . S a

headS :: Stream a -> a
headS (Cofix (S a _)) = a

tailS :: Stream a -> Stream a
tailS (Cofix (S _ r)) = r


iterateS :: (a -> a) -> a -> Stream a
iterateS f = ana' coalg
  where
    coalg a = S a (f a)

takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS n (Cofix (S a r)) = a : takeS (n - 1) r


stream :: Stream Int
stream = iterateS (+1) 0


-- | Hylomorphism

hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = cata f . ana g

data TreeF a r = Leaf a | Node r r deriving (Show, Eq)

instance Functor (TreeF a) where
  fmap _ (Leaf a)   = Leaf a
  fmap f (Node r r') = Node (f r) (f r')

type Tree a = Fix (TreeF a)

leaf :: a -> Tree a
leaf = Fix . Leaf

node :: Tree a -> Tree a -> Tree a
node t1 t2 = Fix (Node t1 t2)

size :: Tree a -> Int
size = cata alg
  where
    alg :: TreeF a Int -> Int
    alg (Leaf _)   = 1
    alg (Node r r') = 1 + r + r'

depth :: Tree a -> Int
depth = cata alg
  where
    alg :: TreeF a Int -> Int
    alg (Leaf _)   = 0
    alg (Node r r') = 1 + max r r'

tree :: Tree Int
tree = node (node (leaf 1) (leaf 2)) (leaf 3)


data LexpF r = Var' String
             | App r r
             | Lam String r
             deriving (Show, Eq)

instance Functor LexpF where
  fmap _ (Var' s)   = Var' s
  fmap f (App r r') = App (f r) (f r')
  fmap f (Lam s r)  = Lam s (f r)

type Lexp = Fix LexpF

type Env' = Map String Lexp

var' :: String -> Lexp
var' = Fix . Var'

app :: Lexp -> Lexp -> Lexp
app e1 e2 = Fix (App e1 e2)

lam :: String -> Lexp -> Lexp
lam s e = Fix (Lam s e)

freeVars' :: Lexp -> Set String
freeVars' = cata alg
  where
    alg :: LexpF (Set String) -> Set String
    alg (Var' s)   = S.singleton s
    alg (App r r') = r `S.union` r'
    alg (Lam s r)  = S.delete s r

subst :: String -> Lexp -> Lexp -> Lexp
subst s e = cata alg
  where
    alg :: LexpF Lexp -> Lexp
    alg (Var' s')   = if s == s' then e else var' s'
    alg (App r r') = app r r'
    alg (Lam s' r)  = lam s' r

printLexp :: Lexp -> String
printLexp = cata alg
  where
    alg :: LexpF String -> String
    alg (Var' s)   = s
    alg (App r r') = r ++ " " ++ r'
    alg (Lam s r)  = "(lambda " ++ s ++ " -> " ++ r ++ ")"

eval' :: Env' -> Lexp -> Lexp
eval' env = cata alg
  where
    alg :: LexpF Lexp -> Lexp
    alg (Var' s)   = M.findWithDefault (var' s) s env
    alg (App r r') = app r r'
    alg (Lam s r)  = lam s r

env' :: Env'
env' = M.fromList [("x", var' "y"), ("y", var' "z"), ("z", var' "x")]

one :: Lexp
one = lam "f" (lam "x" (app (var' "f") (var' "x")))

zero :: Lexp
zero = lam "f" (lam "x" (var' "x"))
