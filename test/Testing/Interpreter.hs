{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Interpreter 求值测试。
--
--   所有用例均以 PhiPlot 源码书写，经 'parseSource' 生成 AST 后求值，
--   测试代码不手工构造 AST：
--
--   * 表达式：@parseExpr@ 把源码补成分号语句，取出 @AExp@ 中的表达式；
--   * 布尔表达式：@parseCond@ 借 @if <cond> {}@ 的条件位取出；
--   * 语句：@parseProgram@ 解析为程序，按块求值。
module Testing.Interpreter where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.IORef
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck (counterexample)
import Test.QuickCheck.Monadic

import Language.PhiPlot.Interpreter as I hiding (run)
import Language.PhiPlot.AST as A
import Language.PhiPlot.Parser (parseSource)

tests :: TestTree
tests = testGroup "Interpreter"
  [
    testEvalExpr,
    testEvalBoolExpr,
    testEvalStmt
  ]

--------------------------------------------------------------------------
-- 求值基建
--------------------------------------------------------------------------

-- | 在全新画布与给定状态下运行一个 EvalM 动作
runEvalM :: EvalState -> EvalM a -> IO (Either Error (a, EvalState))
runEvalM state action = do
  ref <- newIORef Nothing
  let canvas = Canvas ref
  runExceptT (runStateT (runReaderT action canvas) state)

evalExpr :: EvalState -> A.Expr -> IO (Either Error Value)
evalExpr state expr = fmap fst <$> runEvalM state (evalExprM expr)

evalBool :: EvalState -> A.BoolExpr -> IO (Either Error Bool)
evalBool state expr = fmap fst <$> runEvalM state (evalBoolExprM expr)

-- | 按块执行语句序列，返回结束的 flow 与最终状态
evalBlock :: EvalState -> [A.Stmt] -> IO (Either Error (I.Flow, EvalState))
evalBlock state stmts = runEvalM state (evalStmtM (A.Block stmts))

--------------------------------------------------------------------------
-- 源码 -> AST
--------------------------------------------------------------------------

-- | 解析表达式源码（自动补语句分号），如 @1 + 2 * 3@
parseExpr :: String -> Either String A.Expr
parseExpr code = case parseSource (code ++ ";") of
  Left err -> Left $ "parse failed: " ++ show err
  Right (A.Module [A.AExp expr]) -> Right expr
  Right stmts -> Left $ "expected a single expression, but got: " ++ show stmts

-- | 解析布尔表达式源码，借 @if <cond> {}@ 的条件位取出
parseCond :: String -> Either String A.BoolExpr
parseCond code = case parseSource ("if " ++ code ++ " {}") of
  Left err -> Left $ "parse failed: " ++ show err
  Right (A.Module [A.If cond (A.Block []) A.Void]) -> Right cond
  Right stmts -> Left $ "expected a single condition, but got: " ++ show stmts

-- | 解析完整程序
parseProgram :: String -> Either String [A.Stmt]
parseProgram code = case parseSource code of
  Left err -> Left $ "parse failed: " ++ show err
  Right (A.Module stmts) -> Right stmts

--------------------------------------------------------------------------
-- 断言
--------------------------------------------------------------------------

-- | 断言求值以错误结束，且错误满足谓词
assertFails :: Show a => IO (Either Error a) -> (Error -> Bool) -> Assertion
assertFails action ok = do
  result <- action
  case result of
    Left err -> assertBool ("unexpected error: " ++ show err) (ok err)
    Right value -> assertFailure $ "expected an error, but got " ++ show value

isTypeErr :: Error -> Bool
isTypeErr (TypeErr _) = True
isTypeErr _ = False

-- | 断言表达式源码在空状态下的求值结果
assertExprEval :: String -> Value -> Assertion
assertExprEval = assertExprEvalIn emptyState

-- | 断言表达式源码在给定状态下的求值结果
assertExprEvalIn :: EvalState -> String -> Value -> Assertion
assertExprEvalIn state code expected = case parseExpr code of
  Left msg -> assertFailure msg
  Right expr -> do
    actual <- evalExpr state expr
    actual @?= Right expected

-- | 断言表达式求值以指定错误失败
assertExprFails :: String -> (Error -> Bool) -> Assertion
assertExprFails code ok = case parseExpr code of
  Left msg -> assertFailure msg
  Right expr -> assertFails (evalExpr emptyState expr) ok

-- | 断言布尔表达式源码的求值结果
assertBoolEval :: String -> Bool -> Assertion
assertBoolEval code expected = case parseCond code of
  Left msg -> assertFailure msg
  Right cond -> do
    actual <- evalBool emptyState cond
    actual @?= Right expected

-- | 断言布尔表达式求值以指定错误失败
assertBoolFails :: String -> (Error -> Bool) -> Assertion
assertBoolFails code ok = case parseCond code of
  Left msg -> assertFailure msg
  Right cond -> assertFails (evalBool emptyState cond) ok

-- | 断言程序以给定 flow 结束，且执行后各变量取值符合预期
--   （绑定值为 'Nothing' 表示该变量未绑定）
assertStmts :: String -> I.Flow -> [(A.Name, Maybe Value)] -> Assertion
assertStmts code expectedFlow bindings = case parseProgram code of
  Left msg -> assertFailure msg
  Right stmts -> do
    result <- evalBlock emptyState stmts
    case result of
      Left err -> assertFailure $ "evaluation failed: " ++ show err
      Right (flow, finalState) -> do
        flow @?= expectedFlow
        mapM_ (\(name, expected) -> lookupVar name finalState @?= expected) bindings

-- | 断言程序求值以指定错误失败
assertStmtFails :: String -> (Error -> Bool) -> Assertion
assertStmtFails code ok = case parseProgram code of
  Left msg -> assertFailure msg
  Right stmts -> assertFails (evalBlock emptyState stmts) ok

-- | 执行程序并返回最终状态；要求以 'I.Normal' 结束，否则断言失败。
--   用作其它用例的 setup（如先定义函数、再求值表达式）。
execCode :: String -> IO EvalState
execCode code = case parseProgram code of
  Left msg -> assertFailure msg >> pure emptyState
  Right stmts -> do
    result <- evalBlock emptyState stmts
    case result of
      Right (I.Normal, finalState) -> pure finalState
      Right (flow, _) -> assertFailure ("unexpected flow: " ++ show flow) >> pure emptyState
      Left err -> assertFailure ("evaluation failed: " ++ show err) >> pure emptyState

-- | QuickCheck 版：断言表达式源码的求值结果
exprEvaluatesTo :: String -> Value -> Property
exprEvaluatesTo code expected = case parseExpr code of
  Left msg -> counterexample msg False
  Right expr -> ioProperty $ do
    actual <- evalExpr emptyState expr
    pure $ actual === Right expected

--------------------------------------------------------------------------
-- 表达式求值
--------------------------------------------------------------------------

testEvalExpr :: TestTree
testEvalExpr = testGroup "eval expr" $
  [testEvalExprImm] ++ testEvalExprBinOp ++ [testEvalExprCases]

testEvalExprImm = 
  QC.testProperty "Imm x -> VScalar x" $
    \(x :: Double) -> exprEvaluatesTo (show x) (VScalar x)

makeEvalBinOp (symbol, f) = 
  QC.testProperty ("evaluate " ++ symbol) $
    \(x :: Double, y :: Double) -> exprEvaluatesTo
      (show x ++ " " ++ symbol ++ " " ++ show y)
      (VScalar $ f x y)

testEvalExprBinOp = map makeEvalBinOp
  [
    ("+", (+)),
    ("-", (-)),
    ("*", (*))
  ]

testEvalExprCases :: TestTree
testEvalExprCases = testGroup "cases"
  [ testCase "Var" $ do
      state <- execCode "x = 42;"
      assertExprEvalIn state "x" (VScalar 42),
    testCase "unbound Var" $
      assertExprFails "nope" (== Unbound "nope"),
    testCase "UniOp Negative" $
      assertExprEval "-3" (VScalar (-3)),
    testCase "UniOp Positive" $
      assertExprEval "+3" (VScalar 3),
    testCase "BinOp Div" $
      assertExprEval "6 / 4" (VScalar 1.5),
    testCase "BinOp Pow" $
      assertExprEval "2 ** 10" (VScalar 1024),
    testCase "call sin" $
      assertExprEval "sin(0)" (VScalar 0),
    testCase "call sqrt" $
      assertExprEval "sqrt(9)" (VScalar 3),
    testCase "call pow" $
      assertExprEval "pow(2, 10)" (VScalar 1024),
    testCase "call ln" $
      assertExprEval "ln(1)" (VScalar 0),
    testCase "call unknown function" $
      assertExprFails "nope()" (== UnknownFn "nope"),
    testCase "builtin arity: too few" $
      assertExprFails "sin()" (== ArityErr "Wrong number of arguments" 1 0),
    testCase "builtin arity: too many" $
      assertExprFails "sin(0, 1)" (== ArityErr "Wrong number of arguments" 1 2),
    testCase "call user-defined function" $ do
      state <- execCode "def twice(x) { return x * 2; }"
      assertExprEvalIn state "twice(21)" (VScalar 42)
  ]

--------------------------------------------------------------------------
-- 布尔表达式求值
--------------------------------------------------------------------------

testEvalBoolExpr :: TestTree
testEvalBoolExpr = testGroup "eval bool expr"
  [ testCase "BoolAtom True" $
      assertBoolEval "true" True,
    testCase "BoolAtom False" $
      assertBoolEval "false" False,
    testEvalCmp,
    testCase "LogicOp AND" $
      assertBoolEval "true && false" False,
    testCase "LogicOp OR" $
      assertBoolEval "false || true" True,
    testCase "Not" $
      assertBoolEval "!false" True,
    testCase "Nonzero: zero" $
      assertBoolEval "0" False,
    testCase "Nonzero: negative fraction" $
      assertBoolEval "-0.5" True,
    testCase "compare with unbound variable" $
      assertBoolFails "nope < 1" (== Unbound "nope")
  ]

testEvalCmp :: TestTree
testEvalCmp = testGroup "Cmp" $ map makeCmpCase
  [
    ("<", 1, 2, True),
    ("<", 2, 1, False),
    ("<=", 2, 2, True),
    ("<=", 2, 1, False),
    (">", 1, 2, False),
    (">", 2, 1, True),
    (">=", 2, 2, True),
    (">=", 1, 2, False),
    ("==", 2, 2, True),
    ("==", 2, 3, False),
    ("!=", 2, 3, True),
    ("!=", 2, 2, False)
  ]
  where
    makeCmpCase :: (String, Double, Double, Bool) -> TestTree
    makeCmpCase (op, lhs, rhs, expected) = do
      let code = show lhs ++ " " ++ op ++ " " ++ show rhs
      testCase code $ assertBoolEval code expected

--------------------------------------------------------------------------
-- 语句求值
--------------------------------------------------------------------------

testEvalStmt :: TestTree
testEvalStmt = testGroup "eval stmt"
  [ -- 赋值语句
    testCase "assign scalar" $
      assertStmts "x = 1;" I.Normal
        [("x", Just (VScalar 1))],
    testCase "assign pair (Pair evaluation)" $
      assertStmts "p = (1, 2);" I.Normal
        [("p", Just (VPair 1 2))],
    testCase "assign overwrites builtin variable" $
      assertStmts "origin = (1, 2);" I.Normal
        [("origin", Just (VPair 1 2))],
    testCase "assign evaluates rhs in current state" $
      assertStmts "x = 1; y = x + 1;" I.Normal
        [("x", Just (VScalar 1)), ("y", Just (VScalar 2))],
    testCase "assign with unbound rhs" $
      assertStmtFails "x = nope;" (== Unbound "nope"),

    -- 块语句
    testCase "block runs statements in order" $
      assertStmts "x = 1; x = x + 1;" I.Normal
        [("x", Just (VScalar 2))],
    testCase "expression statement is Normal" $
      assertStmts "1;" I.Normal [],

    -- 条件语句
    testCase "if takes then branch" $
      assertStmts "x = 1; if x > 0 { y = 1; } else { y = 2; }" I.Normal
        [("x", Just (VScalar 1)), ("y", Just (VScalar 1))],
    testCase "if takes else branch" $
      assertStmts "x = -1; if x > 0 { y = 1; } else { y = 2; }" I.Normal
        [("x", Just (VScalar (-1))), ("y", Just (VScalar 2))],
    testCase "if with bare condition" $
      assertStmts "if 0 { y = 1; } else { y = 2; }" I.Normal
        [("y", Just (VScalar 2))],

    -- 循环语句
    testCase "for accumulates with positive step" $
      assertStmts "acc = 0; for i from 0 to 4 step 1 { acc = acc + i; }" I.Normal
        [("acc", Just (VScalar 6)), ("i", Just (VScalar 3))],
    testCase "for with negative step" $
      assertStmts "acc = 0; for i from 4 to 0 step -1 { acc = acc + i; }" I.Normal
        [("acc", Just (VScalar 10)), ("i", Just (VScalar 1))],
    testCase "for stops on break" $
      assertStmts
        "acc = 0; for i from 0 to 4 step 1 { if i == 2 { break; } acc = acc + 1; }"
        I.Break
        [("acc", Just (VScalar 2)), ("i", Just (VScalar 2))],
    testCase "for propagates return" $
      assertStmts
        "for i from 0 to 4 step 1 { if i == 2 { return i; } }"
        (I.Return (VScalar 2))
        [("i", Just (VScalar 2))],

    -- 函数定义
    testCase "def installs closure" $ do
      state <- execCode "def f(x) { return x; }"
      case Map.lookup "f" (_functions state) of
        Just (VClosure closure) -> do
          _clParams closure @?= ["x"]
          _clBody closure @?= A.Block [A.Return (A.Var "x")]
        other -> assertFailure $ "expected a closure, but got " ++ show other,
    testCase "def rejects overriding builtin" $
      assertStmtFails "def sin() { }"
        (== RuntimeError "Cannot override builtin function: sin"),
    testCase "arithmetic on non-scalar value -> TypeErr" $
      assertStmtFails "def u() { }\nx = 1 + u();" isTypeErr
  ]
