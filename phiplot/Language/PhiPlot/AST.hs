{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.PhiPlot.AST
  ( Stmt (..),
    Expr (..),
    BoolExpr (..),
    BinaryOperator (..),
    UnaryOperator (..),
  )
where

import GHC.Generics
import Text.PrettyPrint.GenericPretty
import Prelude hiding (Ord (..))

-- 一元操作符
data UnaryOperator
  = Negative
  | Positive
  | NOT
  deriving (Eq, Show, Generic, Out)

-- 二元操作符
data BinaryOperator
  = Plus
  | Minus
  | Mul
  | Div
  | Pow
  | LT
  | GT
  | LE
  | GE
  | EQ
  | NE
  | AND
  | OR
  deriving (Eq, Show, Generic, Out)

type Name = String

data Expr
  = Var Name
  | Imm Double
  | Pair Expr Expr
  | UniOp {uop :: UnaryOperator, exp :: Expr}
  | BinOp {bop :: BinaryOperator, lhs :: Expr, rhs :: Expr}
  | Call {fn :: Name, args :: [Expr]}
  deriving (Eq, Show, Generic, Out)

data BoolExpr
  = Const Bool
  | Cmp BinaryOperator BoolExpr BoolExpr
  | LogicOp BinaryOperator BoolExpr BoolExpr
  | Not BoolExpr
  | Nonzero Expr
  | BEAtom Expr
  deriving (Eq, Show, Generic, Out)

data Stmt
  = Assign Name Expr
  | Def Name [Expr] Stmt
  | For Name Expr Expr Expr Stmt
  | If BoolExpr Stmt Stmt
  | Block [Stmt]
  | Break
  | Return Expr
  | AExp Expr
  | BExp BoolExpr
  | Void
  deriving (Eq, Show, Generic, Out)