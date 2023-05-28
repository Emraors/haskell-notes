module ReaderMonad () where

import Control.Monad.Reader

type VarName = String

type Vars = [VarName]

data Term = Var VarName
    | Lambda VarName Term
    | App Term Term deriving (Eq, Show)

type Env = [(VarName, Term)]

type EvalM a = Reader Env a

eval :: Term -> EvalM Term
eval (Var x) = do
  env <- ask
  case lookup x env of
    Just v -> return v
    Nothing -> return (Var x)
eval (Lambda x body) = do
  env <- ask
  return $ Lambda x (runReader (eval body) ((x, Var x) : env))
eval (App func arg) = do
  func' <- eval func
  arg' <- eval arg
  case func' of
    Lambda x body -> local ((x, arg') :) (eval body)
    _ -> return $ App func' arg'

initialEnv :: Env
initialEnv = []

evalExpr :: Term -> Term
evalExpr expr = runReader (eval expr) initialEnv



and' :: Term
and' = Lambda "x" (Lambda "y" (App (App (Var "x") (Var "y")) (Var "x")))

true' :: Term
true' = Lambda "x" (Lambda "y" (Var "x"))

false' :: Term
false' = Lambda "x" (Lambda "y" (Var "y"))


zero' :: Term
zero' = Lambda "f" (Lambda "x" (Var "x"))

one'' :: Term
one'' = Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))

succ' :: Term
succ' = Lambda "n" (Lambda "g" (Lambda "y" (App (Var "g") (App (App (Var "n") (Var "g")) (Var "y")))))


one' :: Term
one' = App succ' zero'
