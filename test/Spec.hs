import Test.HUnit
import Eval (alphaReduce, topEval)
import Parse
import Data.Map
import Control.Monad.Trans.Except ( runExceptT, ExceptT, throwE )
import Text.Parsec
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans (lift)

myTest :: Test
myTest = TestCase $ assertEqual "This should really work" 3 3

testReduceIdLambda :: Test
testReduceIdLambda = let expr = Lambda "x" (Var "x")
                         got = topEval expr
                         want = expr
                         in
                             TestCase $ assertEqual "eval of simple lambda" got want

testReduceVar :: Test
testReduceVar = let expr = Var "x"
                    got = topEval expr
                    want = expr
                    in
                        TestCase $ assertEqual "eval of single var" got want

testReduceNestedLambda :: Test
testReduceNestedLambda = let expr = Lambda "x" (App (Lambda "y" (Var "y")) (Var "z"))
                             got = topEval expr
                             want = Lambda "x" (Var "z")
                             in
                                TestCase $ assertEqual "eval of nested lambda" got want

type ForcedTest a = ExceptT ParseError IO a

forceParse :: String -> ForcedTest Expr
forceParse s = case Parse.parseExpr s of
                    Left err -> throwE err
                    Right v -> return v

runForcedTest :: ForcedTest a -> IO a
runForcedTest ft = do res <- runExceptT ft
                      case res of
                            Left err -> assertFailure (show err)
                            Right v -> return v

testReduceChurchNumeralsAddition :: Test
testReduceChurchNumeralsAddition = TestCase $ 
                                        runForcedTest $ do
                                                    expr <-  forceParse "(\\m.\\n.\\f.\\x.m f (n f x)) (\\f.\\x.f (f (f x))) (\\f.\\x.f (f (f x)))"
                                                    want <-  forceParse "(\\f0.\\x.f0 (f0 (f0 (f0 (f0 (f0 x))))))"
                                                    let got = topEval expr
                                                        in
                                                        liftIO $ assertEqual "church numeral addition" got want

testReduceChurchNumeralsMult = TestCase $
                                    runForcedTest (do
                                                        expr <- forceParse "(\\m.\\n.\\f.\\x.m (n f) x) (\\f.\\x.f (f (f x))) (\\f.\\x.f (f (f x)))"
                                                        want <- forceParse "(\\f0.\\x.f0 (f0 (f0 (f0 (f0 (f0 (f0 (f0 (f0 x)))))))))"
                                                        let got = topEval expr
                                                            in
                                                                liftIO $ assertEqual "church numeral multiplication" got want)

testAlphaReduceVar = TestCase $
                                let expr = Var "x"
                                    want = Var "x0"
                                    new = "x0"
                                    old = "x"
                                    in 
                                        assertEqual "alpha reduce" (alphaReduce expr old new) want

testAlphaReduceLambda = TestCase $
                                    let expr = Lambda "x" (Lambda "y" (App (Var "y") (Var "x")))
                                        want = Lambda "x0" (Lambda "y" (App (Var "y") (Var "x0")))
                                        old = "x"
                                        new = "x0"
                                        in
                                            assertEqual "alpha reduce" (alphaReduce expr old new) want

testBetaReduceNameConflict = TestCase $
                                        let expr = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (Var "x"))
                                            want = Lambda "x0" (Lambda "x" (Var "x"))
                                            in
                                                assertEqual "beta reduce name conflict" want (topEval expr)

testBetaReduceInnerConflict = TestCase $
                                            let expr = App (Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))) (Var "x")
                                                want = Lambda "x0" (App (Var "x") (Var "x0"))
                                                got = topEval expr
                                                in
                                                    assertEqual "beta reduce inner name conflict" want got

tests :: [Test]
tests = [
    testReduceIdLambda,
    testReduceVar,
    testReduceChurchNumeralsAddition,
    testReduceChurchNumeralsMult,
    testAlphaReduceVar,
    testAlphaReduceLambda,
    --testBetaReduceNameConflict
    testBetaReduceInnerConflict]

main :: IO Counts
main = runTestTT $ TestList tests