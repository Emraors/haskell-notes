{-# OPTIONS_GHC -Wno-type-defaults #-}
module LambdaCalculus () where

import Data.List (nub, (\\), union)


type VarName = String

type Vars = [VarName]

data Term = Var VarName
    | Lambda VarName Term
    | App Term Term deriving (Eq, Show)

freeVars :: Term -> Vars
freeVars (Var x) = [x]
freeVars (Lambda x t1) = nub (freeVars t1) \\ [x]
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2

subst :: VarName -> Term -> Term -> Term
subst x (Var v) newVal = if x == v then newVal else Var v
subst x (Lambda y t1) newVal
    | x == y  = Lambda y t1
    | y `elem` freeVars newVal =
        let z = freshVarName $ usedVars t1 `union` usedVars newVal
            t1' = renameVar y z t1
        in Lambda z $ subst x t1' $ renameVar y z newVal
    | otherwise = Lambda y $ subst x t1 newVal
subst x (App t1 t2) newVal = App (subst x t1 newVal) (subst x t2 newVal)

freshVarName :: Vars -> VarName
freshVarName vars = head $ filter (`notElem` vars) allVarNames

renameVar :: VarName -> VarName -> Term -> Term
renameVar oldName newName (Var v) = if v == oldName then Var newName else Var v
renameVar oldName newName (Lambda v t) =
  if  v == oldName then Lambda v t
  else Lambda v (renameVar oldName newName t)
renameVar oldName newName (App t1 t2) =
    App (renameVar oldName newName t1) (renameVar oldName newName t2)

usedVars :: Term -> [VarName]
usedVars (Var v) = [v]
usedVars (Lambda v t) = v : usedVars t
usedVars (App t1 t2) = usedVars t1 `union` usedVars t2

allVarNames :: [VarName]
allVarNames = ["x" ++ show n | n <- [1..]]

isValue :: Term -> Bool
isValue (Lambda _ _) = True
isValue _ = False


{-
betaReduce :: Term -> Maybe Term
betaReduce (App (Lambda x t1) t2) = Just $ subst x t1 t2
betaReduce (App t1 t2) = App <$> betaReduce t1 <*> betaReduce t2
betaReduce (Lambda x t) = Lambda x <$> betaReduce t
betaReduce _ = Nothing

termEval :: Term -> Term
termEval t = maybe t termEval (betaReduce t)
-}

betaReduce :: Term -> Term
betaReduce (App (Lambda x t1) t2) = subst x t1 t2
betaReduce (App t1 t2) = App (betaReduce t1) (betaReduce t2)
betaReduce (Lambda x t) = Lambda x (betaReduce t)
betaReduce t = t

termEval :: Term -> Term
termEval t =
    let t' = betaReduce t
    in if t' == t then t else termEval t'

and' :: Term
and' = Lambda "x" (Lambda "y" (App (App (Var "x") (Var "y")) (Var "x")))

true' :: Term
true' = Lambda "x" (Lambda "y" (Var "x"))

false' :: Term
false' = Lambda "x" (Lambda "y" (Var "y"))


andTest1 :: Term
andTest1 = App (App and' true') false'



zero' :: Term
zero' = Lambda "f" (Lambda "x" (Var "x"))

one'' :: Term
one'' = Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))

succ' :: Term
succ' = Lambda "n" (Lambda "g" (Lambda "y" (App (Var "g") (App (App (Var "n") (Var "g")) (Var "y")))))


one' :: Term
one' = App succ' zero'


prova :: Term
prova = Lambda "f" (Lambda "x" (App (Var "f") (App (App zero' (Var "f")) (Var "x"))))


type Env = [(VarName, Term)]

-- Evaluate a lambda calculus expression with the given environment
eval :: Env -> Term -> Term
eval env (Var x) = case lookup x env of
  Just v -> v
  Nothing -> Var x
eval env (Lambda x body) = Lambda x (eval env body)
eval env (App func arg) = case eval env func of
  Lambda x body -> eval ((x, eval env arg) : env) body
  func' -> App func' (eval env arg)


lambdaTerm :: Term
lambdaTerm = App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Lambda "z" (Var "z"))

-- Example environment
environment :: Env
environment = [] --[("z", Var "w")]

-- Evaluate the lambda term with the environment
result :: Term
result = eval environment lambdaTerm


rr = App (Lambda "x" (Lambda "y" (Var "y"))) (Lambda "x" (Var "y"))

lctest = App  (Lambda "y" (Var "y")) (Var "x")
