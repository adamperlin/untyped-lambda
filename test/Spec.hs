import Test.HUnit
import Eval
import Parse
import Data.Map

myTest :: Test
myTest = TestCase $ assertEqual "This should really work" 3 3

testEvalUnboundVar = let expr = Var 'x'
                         got = runInterp (eval expr) empty
                         want = (Left $ Failure $ "unbound variable x", empty)
                         in 
                            TestCase $ assertEqual "eval of unbound variable" got want

testEvalBoundVar = let expr = Bind 'x'
                       got = runInterp (eval expr) empty
                       want = (Right $ Res 'x' (Closure (Bind 'x', fromList [])), fromList [('x',Closure (Bind 'x',fromList []))])
                       in 
                            TestCase $ assertEqual "eval of bound variable" got want


testEvalLambda = let expr = Lambda (Var 'x') (Var 'x')
                     got = runInterp (eval expr) empty
                     want = (Right $ Res 'x' (Closure (Var 'x', fromList [])), fromList [])
                     in
                         TestCase $ assertEqual "eval of lambda" got want

testEvalIDLambda = let expr = App (Lambda (Var 'x') (Var 'x')) (Lambda (Var 'x') (Var 'x'))
                       got = runInterp (eval expr) empty
                       want = (Right $ Res 'x' (Closure (Var 'x', fromList [])), fromList [])
                       in
                           TestCase $ assertEqual "eval id lambda " got want

tests :: [Test]
tests = [
    testEvalUnboundVar, 
    testEvalBoundVar,
    testEvalLambda,
    testEvalIDLambda]

main :: IO Counts
main = runTestTT $ TestList tests
