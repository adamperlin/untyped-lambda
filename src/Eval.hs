
module Eval where

import Prelude hiding (lookup)
import Parse
import Data.Char
import Data.Set

newtype EvalError = Failure String deriving (Show, Eq)

subst :: Expr -> String -> Expr -> Expr
subst what for exp = 
    case exp of
        var@(Var v) -> if v == for then what else var
        lam@(Lambda nv e) -> if nv /= for then Lambda nv (subst what for e)
                                else lam
        App e1 e2 -> App (subst what for e1) (subst what for e2)

isBetaReducible :: Expr -> Bool
isBetaReducible (Var v) = False
isBetaReducible (App e1 e2) = 
    case e1 of
        Lambda _ _ -> True
        _ -> isBetaReducible e1 || isBetaReducible e2
isBetaReducible (Lambda nv e) = isBetaReducible e     

betaReduce :: Expr -> Expr
betaReduce var@(Var v) = var
betaReduce l@(Lambda nv e) = Lambda nv (betaReduce e)
betaReduce app@(App e1 e2) = --let (r1, r2) = (betaReduce e1, betaReduce e2)
                                 let r2 = e2
                                     r1' = renameIfConflict e1 $ findFreeReferences r2 empty `union` findFreeReferences e1 empty
                                 in
                                     case r1' of
                                         Lambda nv e -> subst r2 nv e
                                         _ -> App (betaReduce r1') (betaReduce r2)

splitName :: String -> (String, String)
splitName s = if not . isDigit $ last s then
                    (s, "")
                else
                    let (s1, s2) = splitName (init s) in
                        (s1, s2 ++ [last s])

newName :: String -> String
newName "" = ""
newName s = if isDigit (last s) then
                let (s', nStr) = splitName s
                in
                    s' ++ show ((read nStr :: Int) + 1)
                else
                    s ++ "0"

rename :: String -> Set String -> String
rename s conflicts
    | s `member` conflicts = rename (newName s) conflicts
    | otherwise = s

alphaReduce :: Expr -> String -> String -> Expr
alphaReduce var@(Var v) old new = 
    if v == old then Var new
    else
        var

alphaReduce lam@(Lambda nv e) old new
    | nv == new = let nv' = newName nv
        in alphaReduce (Lambda nv' (alphaReduce e nv nv')) old new
    | nv == old = Lambda new (alphaReduce e old new)
    | otherwise = Lambda nv (alphaReduce e old new)

alphaReduce app@(App e1 e2) old new = App (alphaReduce e1 old new) (alphaReduce e2 old new)

findFreeReferences :: Expr -> Set String -> Set String
findFreeReferences (Var v) bound
    | v `member` bound = empty
    | otherwise = insert v empty

findFreeReferences (Lambda nv e) bound = findFreeReferences e (insert nv bound)
findFreeReferences (App e1 e2) bound = refs1 `union` refs2
                                        where refs1 = findFreeReferences e1 bound
                                              refs2 = findFreeReferences e2 bound

renameIfConflict :: Expr -> Set String -> Expr
renameIfConflict expr conflicts = case expr of
                            lam@(Lambda nv e) ->
                                let nv' = rename nv conflicts in
                                    if nv' == nv then
                                        Lambda nv (renameIfConflict e conflicts)
                                    else
                                        Lambda nv' $ renameIfConflict (alphaReduce e nv nv') conflicts

                            App left right -> App (renameIfConflict left conflicts) (renameIfConflict right conflicts)
                            var@(Var _) -> var

topEval :: Expr -> Expr
topEval expr = if isBetaReducible expr then
                    topEval (betaReduce expr)
                else
                    expr

toString :: Expr -> String
toString (Var v) = v
toString (Lambda nv e) = "(\\" ++ nv ++ "." ++ toString e ++ ")"
toString (App e1 e2) = "(" ++ toString e1 ++ " " ++ toString e2 ++ ")"