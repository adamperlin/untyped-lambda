module Main where

import System.IO
import Control.Monad.IO.Class
import Parse
import Eval
import Data.Map hiding (null)
import Control.Monad.State.Lazy (State, runState)
import Control.Monad.Trans.Except ()
import Control.Monad.Trans.State


isQuit :: String -> Bool
isQuit = (`elem` ["quit", "exit", "q"])

doEval :: Expr -> IO ()
doEval = putStrLn . toString . topEval

interpLoop :: IO ()
interpLoop = do
                liftIO $ hSetBuffering stdout NoBuffering
                liftIO $ putStr "λ> "
                line <- liftIO getLine
                if isQuit line then
                        return ()
                else if null line then
                    interpLoop
                else
                        let result = parseExpr line
                                in 
                                    do 
                                        case result of
                                            Left err -> print err >> interpLoop
                                            Right expr -> do
                                                    --print expr
                                                    doEval expr
                                                    interpLoop

main = interpLoop
