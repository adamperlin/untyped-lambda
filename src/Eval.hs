
module Eval where

import Prelude hiding (lookup)
import Parse 
import Data.Map
import Control.Monad.State.Lazy ( State, modify, get, put, runState)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.State.Lazy as LT

--toString :: Closure -> String
--toString (expr, scope) = case expr of
        --Var v -> case lookup v scope of
                    --Just Unbound -> v : ""
                    --Just e -> toString (e, scope)
                    --Nothing -> v : ""
        --Lambda (Var v) e -> "(" ++ '\\' : v : ("." ++ toString (e, scope)) ++ ")"
        --App e1 e2 -> toString (e1, scope) ++ " " ++ toString (e2, scope)
        --Unit -> "()"

--toString :: Result -> String
--toString (Res v (Closure p env)) = case lookup v env of
--                                    Just c -> toString $ Res 
 --                                   Nothing -> "(\\" ++ [v] ++ toString (Res )




newtype EvalError = Failure String deriving (Show, Eq)

type Env = Map Char Closure
data Closure =  Closure (Expr, Env) deriving (Show, Eq)
data Result = Res Char Closure deriving (Show, Eq)
type InterpMonad = ExceptT EvalError (State Env) Result

toStringResult :: Result -> String
toStringResult (Res v clos@(Closure (Bind b, env))) = b : ""
toStringResult (Res v clos@(Closure (e, env))) = case lookup v env of
                                                Nothing -> "(\\" ++ [v] ++ "." ++ (toStringClosure clos) ++ ")"
                                                _ -> toStringClosure clos

toStringClosure :: Closure -> String
toStringClosure (Closure (expr, env)) = case expr of
                                    Var v -> case lookup v env of
                                                Just c -> toStringClosure c
                                                Nothing -> v : ""
                                    Bind v -> v : ""
                                    Lambda (Var p) e -> "(\\" ++ [p] ++ "." ++ toStringClosure (Closure (e, env)) ++ ")"
                                    App e1 e2 -> toStringClosure (Closure (e1, env)) ++ " " ++ toStringClosure (Closure (e2, env))

eval :: Expr -> InterpMonad
eval (Var v) = do
                    env <- get
                    case lookup v env of
                        Nothing -> throwError $ Failure $ "unbound variable " ++ [v]
                        Just (Closure (expr, cEnv)) -> do
                                                            put cEnv
                                                            e <- eval expr
                                                            put env
                                                            return e

eval (Bind b) = let c = Closure (Bind b, empty) in modify (insert b c) >> (return $ Res b c)

eval (Lambda (Var param) body) = do
                                    oldEnv <- get
                                    modify (delete param)
                                    newEnv <- get
                                    put oldEnv
                                    return $ Res param (Closure (body, newEnv))

eval (App e1 e2) = do
                        prevEnv <- get
                        Res v2 c2 <- eval e2
                        Res v1 (Closure (cExpr1, cEnv1)) <- eval e1
                        case lookup v1 cEnv1 of
                            Nothing -> do
                                            put cEnv1
                                            modify (insert v1 c2)
                                            e <- eval cExpr1
                                            put prevEnv
                                            return e
                            Just e -> throwError $ Failure $ "param " ++ [v1] ++ " already bound"

eval Unit = return $ Res '_' (Closure (Unit, empty))

runInterp :: InterpMonad -> Env -> (Either EvalError Result, Env)
runInterp im prevState = runState (runExceptT im) prevState