{-# LANGUAGE DeriveFunctor #-}

module FreeMonads () where

import Control.Monad.Free

data StackF a
  = Push Int a
  | Pop (Int -> a)
  deriving (Functor)

type Stack = Free StackF

push :: Int -> Stack ()
push n = liftF $ Push n ()

pop :: Stack Int
pop = liftF $ Pop id

runStack :: Stack a -> [Int] -> (a, [Int])
runStack (Pure a) s = (a, s)
runStack (Free (Push n a)) s = runStack a (n : s)
runStack (Free (Pop f)) (s : ss) = runStack (f s) ss
runStack (Free (Pop _)) [] = error "Empty stack"

--- Example of Stack, should return (20, [10]) when run with runStack and the empty list
example :: Stack Int
example = do
  push 10
  push 20
  pop

--- Example of Stack, should return an error when run with runStack
errorExample :: Stack Int
errorExample = do
  push 10
  _ <- pop
  pop

data ExprF a
  = Print String a
  | Read (String -> a)
  | Add Int Int (Int -> a)
  | Mul Int Int (Int -> a)
  deriving (Functor)

type Expr = Free ExprF

print' :: String -> Expr ()
print' s = liftF $ Print s ()

read' :: Expr String
read' = liftF $ Read id

add :: Int -> Int -> Expr Int
add x y = liftF $ Add x y id

mul :: Int -> Int -> Expr Int
mul x y = liftF $ Mul x y id

runExpr :: Show a => Expr a -> IO a
runExpr (Pure a) = return a
runExpr (Free (Print s a)) = putStrLn s >> runExpr a
runExpr (Free (Read f)) = getLine >>= runExpr . f
runExpr (Free (Add x y f)) = runExpr $ add x y >>= f
runExpr (Free (Mul x y f)) = runExpr $ mul x y >>= f

--- Example of Expr, should print "Hello World" and return 42 when run with runExpr

example' :: Expr Int
example' = do
  print' "Hello World"
  x <- add 20 22
  return x

program :: IO Int
program = runExpr example'

data StackOp x a
  = Push' x a
  | Pop' (Maybe x -> a)
  deriving (Functor)

type StackOpFree x a = Free (StackOp x) a

push' :: x -> StackOpFree x ()
push' x = liftF $ Push' x ()

pop' :: StackOpFree x (Maybe x)
pop' = liftF $ Pop' id

runStack' :: [x] -> StackOpFree x a -> (a, [x])
runStack' xs (Pure a) = (a, xs)
runStack' xs (Free (Push' y next)) = runStack' (y : xs) next
runStack' [] (Free (Pop' next)) = runStack' [] (next Nothing)
runStack' (x : xs) (Free (Pop' next)) = runStack' xs (next (Just x))

--- Example of StackOpFree, should return (Just 20, [10]) when run with runStack'

example'' :: StackOpFree Int (Maybe Int)
example'' = do
  push' 10
  push' 20
  pop'

-- Define the DSL using a free monad
data DSLF next
  = PrintLn String next
  | ReadLn (String -> next)
  deriving (Functor)

-- Convenience type alias for the DSL
type DSL = Free DSLF

-- Smart constructors for the DSL operations
printLn' :: String -> DSL ()
printLn' str = liftF (PrintLn str ())

readLn' :: DSL String
readLn' = liftF (ReadLn id)

-- Example program using the DSL
exampleProgram :: DSL ()
exampleProgram = do
  printLn' "What is your name?"
  name <- readLn'
  printLn' $ "Hello, " ++ name ++ "!"

pprint :: Show a => DSL a -> String
pprint (Pure _) = "return"
pprint (Free (PrintLn str next)) = "printLn " ++ show str ++ " >> " ++ pprint next
pprint (Free (ReadLn next)) = "readLn >>= " ++ pprint (next "input")

-- Interpreter for the DSL
interpret :: DSL a -> IO a
interpret (Pure result) = return result
interpret (Free (PrintLn str next)) = do
  putStrLn str
  interpret next
interpret (Free (ReadLn next)) = do
  input <- getLine
  interpret (next input)
