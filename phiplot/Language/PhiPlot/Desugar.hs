module Language.PhiPlot.Desugar (desugar) where

import Language.PhiPlot.AST
  ( BinaryOperator (..),
    BoolExpr (..),
    Expr (..),
    Stmt (..),
  )

desugarStmt :: Stmt -> Stmt
desugarStmt stmt = case stmt of
  (Def name args (Block xs)) -> Def name args (Block $ desugar xs)
  (If cond t f) -> If cond (desugarStmt t) (desugarStmt f)
  (For var start stop step body) -> For var start stop step (desugarStmt body)
  (Block ss) -> Block $ desugar ss
  anythingElse -> anythingElse

desugar = map desugarStmt