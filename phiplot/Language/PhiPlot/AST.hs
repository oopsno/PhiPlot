{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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
         deriving (Eq, Show, Generic)

data Expr = Var Name
          | Number Double
          | UniOp UniOp Expr
          | BinOp Op Expr Expr
          | Call Name [Expr]
          | Fcall Name [Expr]
          deriving (Eq, Show, Generic)

data BoolExpr = BoolVal Bool
              | Cmp Op BoolExpr BoolExpr
              | LogicOp Op BoolExpr BoolExpr
              | Not BoolExpr
              | Nonzero Expr
              | BEAtom Expr
              deriving (Eq, Show, Generic)

data Op = Plus | Minus
        | Mul  | Div
        | LT | GT | LE | GE | EQ | NE 
        | AND | OR
        deriving (Eq, Show, Generic)

data UniOp = Negative | Positive | NOT deriving (Eq, Show, Generic)

instance Out AST
instance Out Expr
instance Out BoolExpr
instance Out UniOp
instance Out Op
