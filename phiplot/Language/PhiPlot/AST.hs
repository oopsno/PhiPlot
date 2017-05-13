{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.PhiPlot.AST ( AST(..),
                              Expr(..),
                              BoolExpr(..),
                              Op(..),
                              UniOp(..) ) where

import Prelude    hiding ( Ord(..) )
import GHC.Generics
import Text.PrettyPrint.GenericPretty

type Name = String

data AST = Assign Name Expr
         | Def  Name [Expr] [AST]
         | For Name Expr Expr Expr [AST]
         | If BoolExpr [AST] [AST]
         | While BoolExpr [AST]
         | Break
         | Return Expr
         | AExp Expr
         | BExp BoolExpr
         | Void
         deriving (Eq, Show, Generic, Out)

data Expr = Var Name
          | Number Double
          | UniOp UniOp Expr
          | BinOp Op Expr Expr
          | Call Name [Expr]
          | Fcall Name [Expr]
          deriving (Eq, Show, Generic, Out)

data BoolExpr = BoolVal Bool
              | Cmp Op BoolExpr BoolExpr
              | LogicOp Op BoolExpr BoolExpr
              | Not BoolExpr
              | Nonzero Expr
              | BEAtom Expr
              deriving (Eq, Show, Generic, Out)

data Op = Plus | Minus
        | Mul  | Div
        | LT | GT | LE | GE | EQ | NE 
        | AND | OR
        deriving (Eq, Show, Generic, Out)

data UniOp = Negative | Positive | NOT deriving (Eq, Show, Generic, Out)
