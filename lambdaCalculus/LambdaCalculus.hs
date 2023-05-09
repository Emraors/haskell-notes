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

apply :: Term -> Term
apply (App (Lambda x t12) v2@(Lambda _ _)) = subst x t12 v2
apply (App v1@(Lambda _ _) t2) = let t2' = apply t2 in App v1 t2'
apply (App t1 t2) = let t1' = apply t1 in App t1' t2
apply _ = error "Not an application"


lctest1 :: Term
lctest1 = Lambda "f" (Lambda "x" (Var "x"))

lctest2 :: Term
lctest2 = App (App lctest1 lctest1) (Lambda "y" (Var "y"))

lctest4 :: Term
lctest4 = App (Lambda "x" (Lambda "y" (Var "y"))) (Lambda "x" (Var "y"))

lctest5 :: Term
lctest5 = App (Lambda "x" (Lambda "z" (Var "z"))) (Lambda "x" (Var "y"))
