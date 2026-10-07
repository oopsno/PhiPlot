{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.PhiPlot.AST
  ( Stmt (..),
    Expr (..),
    BoolExpr (..),
    BinaryOperator (..),
    UnaryOperator (..),
    CompareOperator (..),
    LogicalBinaryOperator (..),
    Name,
  )
where

import Data.Bits (And)
import Data.Generics
import qualified Data.Generics.Aliases as Data
import GHC.Base (TrName (TrNameD))
import GHC.Generics
import Text.PrettyPrint.GenericPretty
import Prelude hiding (Ord (..))

-- 一元操作符
data UnaryOperator
  = Negative
  | Positive
  deriving (Eq, Show, Data, Typeable, GHC.Generics.Generic, Out)

-- 二元操作符
data BinaryOperator
  = Plus
  | Minus
  | Mul
  | Div
  | Pow
  deriving (Eq, Show, Data, Typeable, GHC.Generics.Generic, Out)

data CompareOperator
  = LT
  | GT
  | LE
  | GE
  | EQ
  | NE
  deriving (Eq, Show, Data, Typeable, GHC.Generics.Generic, Out)

data LogicalBinaryOperator
  = AND
  | OR
  deriving (Eq, Show, Data, Typeable, GHC.Generics.Generic, Out)

type Name = String

data Expr
  = Var Name
  | Imm Double
  | Pair Expr Expr
  | UniOp {uop :: UnaryOperator, exp :: Expr}
  | BinOp {bop :: BinaryOperator, lhs :: Expr, rhs :: Expr}
  | Call {fn :: Name, args :: [Expr]}
  deriving (Eq, Show, Data, Typeable, GHC.Generics.Generic, Out)

data BoolExpr
  = BoolAtom Bool
  | Cmp CompareOperator Expr Expr
  | LogicOp LogicalBinaryOperator BoolExpr BoolExpr
  | Not BoolExpr
  | Nonzero Expr
  deriving (Eq, Show, Data, Typeable, GHC.Generics.Generic, Out)

data Stmt
  = Assign {dst :: Name, value :: Expr}
  | Def {fname :: Name, args :: [Name], body :: Stmt}
  | For {var :: Name, start :: Expr, end :: Expr, step :: Expr, body :: Stmt}
  | If {condition :: BoolExpr, thenBody :: Stmt, elseBody :: Stmt}
  | Block [Stmt]
  | Break
  | Return Expr
  | AExp Expr
  | BExp BoolExpr
  | Void
  deriving (Eq, Show, Data, Typeable, GHC.Generics.Generic, Out)