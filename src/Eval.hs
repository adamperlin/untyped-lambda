
module Eval where

import Prelude hiding (lookup)
import Parse
import Data.Char

newtype EvalError = Failure String deriving (Show, Eq)

subst :: Expr -> String -> Expr -> Expr
subst what for exp = 
    case exp of
        var@(Var v) -> if v == for then what else var
        l@(Lambda nv e) -> Lambda nv (subst what for e)
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
betaReduce app@(App e1 e2) = let (r1, r2) = (betaReduce e1, betaReduce e2)
                                 (r1', r2') = (alphaReduceIfConflict r1 r2, r2)
                                 in
                                     case r1' of
                                         Lambda nv e ->
                                             betaReduce (subst r2' nv e)
                                         _ -> App (betaReduce r1') (betaReduce r2')

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

alphaReduceIfConflict :: Expr -> Expr -> Expr
alphaReduceIfConflict e1 e2 = case (e1, e2) of
                                (Lambda nv0 body0, Lambda nv1 body1) ->
                                                        if nv1 == nv0 then
                                                            let nv0' = newName nv0
                                                                newBody = alphaReduce body0 nv0 nv0' in
                                                                Lambda nv0' (alphaReduceIfConflict newBody e2)
                                                        else Lambda nv0 (alphaReduceIfConflict body0 e2)
                                (Lambda nv0 body0, Var v) ->
                                                            if nv0 == v then
                                                                let nv0' = newName nv0
                                                                    newBody = alphaReduce body0 nv0 nv0' in
                                                                    Lambda nv0' (alphaReduceIfConflict newBody e2)
                                                            else Lambda nv0 (alphaReduceIfConflict body0 e2)
                                (App expr1 expr2, _) -> App (alphaReduceIfConflict expr1 e2) (alphaReduceIfConflict expr2 e2)
                                _ -> e1

topEval :: Expr -> Expr
topEval expr = if isBetaReducible expr then
                    topEval (betaReduce expr)
                else
                    expr

toString :: Expr -> String
toString (Var v) = v
toString (Lambda nv e) = "(\\" ++ nv ++ "." ++ toString e ++ ")"
toString (App e1 e2) = "(" ++ toString e1 ++ " " ++ toString e2 ++ ")"