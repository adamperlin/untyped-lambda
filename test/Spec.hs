import Test.HUnit
import Eval
import Parse
import Data.Map

myTest :: Test
myTest = TestCase $ assertEqual "This should really work" 3 3

testEvalUnboundVar = let expr = Var "x"
                         got = runInterp (eval expr) empty
                         want = (Left $ Failure "unbound variable x", empty)
                         in 
                            TestCase $ assertEqual "eval of unbound variable" got want

testEvalBoundVar = let expr = Bind "x"
                       got = runInterp (eval expr) empty
                       want = (Right $ Res $ Closure Nothing (Unit, empty), fromList [("x", Closure Nothing (Bind "x", empty))])
                       in 
                            TestCase $ assertEqual "eval of bound variable" got want


testEvalLambda = let expr = Lambda "x" (Var "x")
                     got = runInterp (eval expr) empty
                     want = (Right $ Res $ Closure (Just "x") (Var "x", empty), empty)
                     in
                         TestCase $ assertEqual "eval of lambda" got want

testEvalIDLambda = let expr = App (Lambda "x" (Var "x")) (Lambda "x" (Var "x"))
                       got = runInterp (eval expr) empty
                       want = (Right $ Res $ Closure (Just "x") (Var "x", empty), empty)
                       in
                           TestCase $ assertEqual "eval id lambda " got want

testEvalPartialApp = let expr = App (Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))) (Lambda "x" (Var "x"))
                         got = runInterp (eval expr) empty
                         want = (Right $ Res $ Closure (Just "x") (App (Var "f") (Var "x"), fromList [ ("f", Closure (Just "x") (Var "x", empty))] ), empty)
                         in
                             TestCase $ assertEqual "partial application" got want

tests :: [Test]
tests = [
    testEvalUnboundVar, 
    testEvalBoundVar,
    testEvalLambda,
    testEvalIDLambda,
    testEvalPartialApp]

main :: IO Counts
main = runTestTT $ TestList tests
