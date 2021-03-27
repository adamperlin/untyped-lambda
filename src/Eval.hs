
module Eval where

import Prelude hiding (lookup)
import Parse 
import Data.Map
import Control.Monad.State.Lazy ( State, modify, get, put, runState)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.State.Lazy as LT

newtype EvalError = Failure String deriving (Show, Eq)

type Env = Map String Closure
data Closure = Closure (Maybe String) (Expr, Env) deriving (Show, Eq)
newtype Result = Res Closure deriving (Show, Eq)
type InterpMonad = ExceptT EvalError (State Env) Result

eval :: Expr -> InterpMonad
eval (Var v) = do
                    env <- get
                    case lookup v env of
                        Nothing -> throwError $ Failure $ "unbound variable " ++ v
                        Just cl -> return $ Res cl

{- (\x.(\x.x)) y -}
{-(\x.x) -> -}
eval (Lambda nv e) = do
                        env <- get
                        return $ Res (Closure (Just nv) (e, env))

eval (App e1 e2) = do
                        Res c1 <- eval e1
                        Res c2 <- eval e2
                        case c1 of
                            Closure (Just nv) (exp, cEnv) -> do
                                                        prevEnv <- get
                                                        put cEnv
                                                        modify (insert nv c2)
                                                        r <- eval exp
                                                        put prevEnv
                                                        return r
                                                       
                            _ -> throwError $ Failure "cannot apply to non-lambda lhs"

eval (Bind b) = do
                    env <- get
                    modify (insert b (Closure Nothing (Bind b, env)))
                    return $ Res $ Closure Nothing (Unit, empty)
                        
printClos :: Closure -> String
printClos (Closure (Just x) (expr, env)) = "(\\" ++ x ++ "." ++ toStringExpr (expr, insert x (Closure Nothing (Bind x, env)) env) ++ ")"
printClos (Closure Nothing (expr, env)) = toStringExpr (expr, env)

toStringExpr :: (Expr, Env) -> String
toStringExpr (Var v, env) = maybe "lookup error" printClos (lookup v env)
toStringExpr (Lambda nv exp, env) = "(\\" ++ nv ++ "." ++ toStringExpr (exp, insert nv (Closure Nothing (Bind nv, env)) env) ++ ")"
toStringExpr (App e1 e2, env) = toStringExpr (e1, env) ++ " " ++ toStringExpr (e2, env)
toStringExpr (Bind b, env) = b
toStringExpr (Unit, _) = "Unit"

runInterp :: InterpMonad -> Env -> (Either EvalError Result, Env)
runInterp = runState . runExceptT