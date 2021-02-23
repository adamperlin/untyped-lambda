module Main where

import System.IO
import Control.Monad.IO.Class
import Parse
import Eval
import Data.Map
import Control.Monad.State.Lazy (State, runState)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State


isQuit :: String -> Bool
isQuit = (`elem` ["quit", "exit", "q"])

doEval :: Expr -> Env -> IO Env 
doEval expr curEnv =  let (res, env') = runInterp (eval expr) curEnv in
                            (case res of
                                Left err -> print err
                                Right r -> (print r) >> (putStrLn $ toStringResult r) )>> return env'
                    

interpLoop :: StateT Env IO ()
interpLoop = do
                liftIO $ hSetBuffering stdout NoBuffering
                curEnv <- get
                liftIO $ putStr "λ> "
                line <- liftIO $ getLine
                if isQuit line then
                        return ()
                else
                        let result = parseExpr line
                                in case result of
                                        Left err -> liftIO (print err) >> interpLoop
                                        Right expr -> do
                                                    liftIO $ print expr
                                                    env' <- liftIO $ doEval expr curEnv
                                                    put env'
                                                    interpLoop


main = runStateT interpLoop empty