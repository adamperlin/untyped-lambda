import Test.HUnit
import Eval (alphaReduce, topEval, findFreeReferences)
import Parse
import Data.Set
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
                                                    want <-  forceParse "(\\f.\\x.f (f (f (f (f (f x))))))"
                                                    let got = topEval expr
                                                        in
                                                        liftIO $ assertEqual "church numeral addition" got want

testReduceChurchNumeralsMult = TestCase $
                                    runForcedTest (do
                                                        expr <- forceParse "(\\m.\\n.\\f.\\x.m (n f) x) (\\f.\\x.f (f (f x))) (\\f.\\x.f (f (f x)))"
                                                        want <- forceParse "(\\f.\\x.f (f (f (f (f (f (f (f (f x)))))))))"
                                                        let got = topEval expr
                                                            in
                                                                liftIO $ assertEqual "church numeral multiplication" got want)

testReduceChurchNumeralsFactorial = TestCase $
                                        runForcedTest (do
                                                            expr <- forceParse "((\\x.(\\y.x (y y)) (\\y.x (y y))) (\\f.\\n.((\\n.n (\\x.\\a.\\b.b) (\\a.\\b.a)) n) (\\f.\\x.f x) ((\\m.\\n.\\f.m (n f)) n (f ((\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) n))))) (\\f.\\x.f (f (f (f x))))"
                                                            want <- forceParse "(\\f.(\\x.(f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))))))))))))))"
                                                            let got = topEval expr
                                                                in
                                                                    liftIO $ assertEqual "church numeral factorial" got want
                                                        )

testReduceChurchNumeralsDivision = TestCase $
                                        runForcedTest (do 
                                                            expr <- forceParse "(\\n.\\m.((\\f.(\\x.f (x x)) (\\x.f (x x))) (\\c.\\n.\\m.\\f.\\x.(\\d.((\\n.n (\\x.\\a.\\b.b) (\\a.\\b.a)) d) ((\\f.\\x.x) f x) (f (c d m f x))) ((\\m.\\n.n (\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) m) n m))) ((\\n.\\f.\\x.f (n f x)) n) m) (\\f.\\x.f (f (f (f (f (f (f (f (f x))))))))) (\\f.\\x.f (f (f x)))"
                                                            want <- forceParse "(\\f.\\x.f (f (f x)))"
                                                            let got = topEval expr
                                                                in
                                                                    liftIO $ assertEqual "church numeral division" got want)

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

testBetaReduceInnerConflict = TestCase $
                                            let expr = App (Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))) (Var "x")
                                                want = Lambda "x0" (App (Var "x") (Var "x0"))
                                                got = topEval expr
                                                in
                                                    assertEqual "beta reduce inner name conflict" want got
{--
"\y.\x.\x0.x y" x
"\x0.\x1.x0 x"
--}
testBetaReduceMutlipleConflicts = TestCase $
                                                let expr = App (Lambda "y" (Lambda "x" (Lambda "x0" (App (Var "x") (Var "y"))))) (Var "x")
                                                    want = Lambda "x0" (Lambda "x1" (App (Var "x0") (Var "x")))
                                                    got = topEval expr
                                                    in
                                                        assertEqual "beta reduce multiple name conflicts" want got

testBetaReduceShadowedBoundVars = TestCase $
                                                let expr = App (Lambda "x" (Lambda "x" (Var "x"))) (Var "x")
                                                    want = Lambda "x0" (Var "x0")
                                                    got = topEval expr
                                                    in
                                                        assertEqual "shadowed bound vars" want got

testFindFreeReferences = TestCase $
                                     let expr = Lambda "x" (App (Lambda "y" (Var "z")) (Var "x"))
                                         want = fromList ["z"]
                                         got = findFreeReferences expr empty
                                         in
                                             assertEqual "find free references" want got
-- (\x.\y.x y) (\z.y z)
testBetaReduceComplexConflict = TestCase $
                                            let expr = App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Lambda "z" (App (Var "y") (Var "z")))
                                                want = Lambda "y0" (App (Var "y") (Var "y0"))
                                                got = topEval expr
                                                in
                                                    assertEqual "more complex free variable conflict" want got

-- (\x.\y.x y0) y
testReduceConflictAfterRename = TestCase $
                                            let expr = App (Lambda "x" (Lambda "y" (App (Var "y") (Var "y0")))) (Var "y")
                                                want = Lambda "y1" (App (Var "y1") (Var "y0"))
                                                got = topEval expr
                                                in
                                                    assertEqual "name conflict after rename" want got

tests :: [Test]
tests = [
    testReduceIdLambda,
    testReduceVar,
    testReduceChurchNumeralsAddition,
    testReduceChurchNumeralsMult,
    testReduceChurchNumeralsFactorial,
    testReduceChurchNumeralsDivision,
    testAlphaReduceVar,
    testAlphaReduceLambda,
    --testBetaReduceNameConflict
    testFindFreeReferences,
    testBetaReduceInnerConflict,
    testBetaReduceMutlipleConflicts,
    testBetaReduceShadowedBoundVars,
    testBetaReduceComplexConflict,
    testReduceConflictAfterRename]

main :: IO Counts
main = runTestTT $ TestList tests