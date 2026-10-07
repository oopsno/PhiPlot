-- | 语句（'Language.PhiPlot.Parser' 中的 @stmt@）的单元测试。
--
--   @stmt@ 本身未对外导出，因此这里统一通过公开入口 'parsePhiplot'
--   驱动解析，并以 @[Stmt]@ 的形式断言解析结果。
module Testing.Parser (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Language.PhiPlot.AST as A
import Language.PhiPlot.Parser (parsePhiplot)

tests :: TestTree
tests = testGroup "Parser" [testStmt, testInvalidStmt]

-- | 断言源码解析出的语句序列与期望一致
parsesTo :: String -> [A.Stmt] -> Assertion
parsesTo source expected = case parsePhiplot source of
  Left err -> assertFailure $ "parse failed: " ++ show err
  Right actual -> actual @?= expected

-- | 断言源码无法被解析
rejects :: String -> Assertion
rejects source = case parsePhiplot source of
  Left _ -> pure ()
  Right actual -> assertFailure $ "expected parse failure, but got " ++ show actual

testStmt :: TestTree
testStmt = testGroup "stmt"
  [ -- 赋值语句
    testCase "assign: dst = src" $
      "x = 1;" `parsesTo` [A.Assign "x" (A.Imm 1)],
    testCase "assign: dst IS src" $
      "x IS 1;" `parsesTo` [A.Assign "x" (A.Imm 1)],
    testCase "assign: identifier is case-insensitive" $
      "X = 1;" `parsesTo` [A.Assign "x" (A.Imm 1)],
    testCase "assign: expression keeps precedence" $
      "x = 1 + 2 * 3;"
        `parsesTo` [A.Assign "x" (A.BinOp A.Plus (A.Imm 1) (A.BinOp A.Mul (A.Imm 2) (A.Imm 3)))],
    testCase "assign: unary minus" $
      "x = -1;" `parsesTo` [A.Assign "x" (A.UniOp A.Negative (A.Imm 1))],

    -- 全局变量（保留字）
    testCase "assign to origin" $
      "origin = (1, 2);"
        `parsesTo` [A.Assign "origin" (A.Pair (A.Imm 1) (A.Imm 2))],
    testCase "assign to scale with IS" $
      "scale IS (2, 3);"
        `parsesTo` [A.Assign "scale" (A.Pair (A.Imm 2) (A.Imm 3))],
    testCase "assign to rot" $
      "rot = 90;" `parsesTo` [A.Assign "rot" (A.Imm 90)],
    testCase "assign to canvasSize" $
      "canvasSize = (64, 64);"
        `parsesTo` [A.Assign "canvassize" (A.Pair (A.Imm 64) (A.Imm 64))],

    -- 表达式语句 / 块语句
    testCase "expression statement" $
      "draw(1, 2);" `parsesTo` [A.AExp (A.Call "draw" [A.Imm 1, A.Imm 2])],
    testCase "all with expression" $
      "draw(x ** y, -f(((y))));" `parsesTo` [A.AExp
        (A.Call "draw" [
          (A.BinOp A.Pow (A.Var "x") (A.Var "y")),
          (A.UniOp A.Negative
            (A.Call "f" [A.Var "y"]))])],
    testCase "empty block" $
      "{}" `parsesTo` [A.Block []],
    testCase "block keeps statement order" $
      "{ x = 1; y = 2; }"
        `parsesTo` [A.Block [A.Assign "x" (A.Imm 1), A.Assign "y" (A.Imm 2)]],

    -- 循环语句
    testCase "for with explicit step" $
      "for i from 0 to 10 step 2 { }"
        `parsesTo` [A.For "i" (A.Imm 0) (A.Imm 10) (A.Imm 2) (A.Block [])],
    testCase "for defaults to step 1" $
      "for i from 0 to 10 { }"
        `parsesTo` [A.For "i" (A.Imm 0) (A.Imm 10) (A.Imm 1) (A.Block [])],
    testCase "for with trailing draw" $
      "for t from 0 to 1 step 0.1 draw(t, t);"
        `parsesTo` [A.For "t" (A.Imm 0) (A.Imm 1) (A.Imm 0.1) (A.AExp (A.Call "draw" [A.Var "t", A.Var "t"]))],
    testCase "for body may nest if" $
      "for i from 0 to 3 { if i > 1 { draw(i, i); } }"
        `parsesTo` [ A.For
                       "i"
                       (A.Imm 0)
                       (A.Imm 3)
                       (A.Imm 1)
                       (A.Block
                          [ A.If
                              (A.Cmp A.GT (A.Var "i") (A.Imm 1))
                              (A.Block [A.AExp (A.Call "draw" [A.Var "i", A.Var "i"])])
                              A.Void
                          ])
                   ],

    -- 条件语句
    testCase "if ... else ..." $
      "if x > 0 { x = 1; } else { x = 2; }"
        `parsesTo` [ A.If
                       (A.Cmp A.GT (A.Var "x") (A.Imm 0))
                       (A.Block [A.Assign "x" (A.Imm 1)])
                       (A.Block [A.Assign "x" (A.Imm 2)])
                   ],
    testCase "if without else" $
      "if x == 0 { }"
        `parsesTo` [A.If (A.Cmp A.EQ (A.Var "x") (A.Imm 0)) (A.Block []) A.Void],
    testCase "if with bare condition -> Nonzero" $
      "if x { }"
        `parsesTo` [A.If (A.Nonzero (A.Var "x")) (A.Block []) A.Void],
    testCase "if with parenthesized condition" $
      "if (a < b) { }"
        `parsesTo` [A.If (A.Cmp A.LT (A.Var "a") (A.Var "b")) (A.Block []) A.Void],
    testCase "bare condition combines with logical operator" $
      "if x && y { }"
        `parsesTo` [A.If (A.LogicOp A.AND (A.Nonzero (A.Var "x")) (A.Nonzero (A.Var "y"))) (A.Block []) A.Void],
    testCase "if nonzero without else" $
      "if x + 42 { }"
        `parsesTo` [A.If (A.Nonzero $ A.BinOp A.Plus (A.Var "x") (A.Imm 42)) (A.Block []) A.Void],
    testCase "if with logical condition" $
      "if a < b && c > d { }"
        `parsesTo` [ A.If
                       (A.LogicOp A.AND (A.Cmp A.LT (A.Var "a") (A.Var "b")) (A.Cmp A.GT (A.Var "c") (A.Var "d")))
                       (A.Block [])
                       A.Void
                   ],

    -- 函数定义（仅出现在顶层）
    testCase "def at top level" $
      "def f(x, y) { return x + y; }"
        `parsesTo` [
          A.Def "f" ["x", "y"]
            (A.Block [
              A.Return (A.BinOp A.Plus (A.Var "x") (A.Var "y"))])],

    -- 顶层多语句与注释
    testCase "multiple statements" $
      "x = 1;\ny = 2;" `parsesTo` [A.Assign "x" (A.Imm 1), A.Assign "y" (A.Imm 2)],
    testCase "line comment" $
      "// comment\nx = 1;" `parsesTo` [A.Assign "x" (A.Imm 1)],
    testCase "block comment" $
      "/* comment */ x = 1;" `parsesTo` [A.Assign "x" (A.Imm 1)]
  ]

testInvalidStmt :: TestTree
testInvalidStmt = testGroup "invalid stmt"
  [ testCase "assignment without rhs" $
      rejects "x = ;",
    testCase "unclosed block" $
      rejects "{ x = 1;",
    testCase "for without bounds" $
      rejects "for i { }"
  ]
