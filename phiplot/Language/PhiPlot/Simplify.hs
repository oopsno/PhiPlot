module Language.PhiPlot.Simplify (simplify) where

import Language.PhiPlot.AST (Stmt (..))
import Data.Generics

simplifyStmt :: Stmt -> Stmt
simplifyStmt = everywhere (mkT f) 
  where
    f :: Stmt -> Stmt
    f (Block []) = Void
    f (Block [Void]) = Void
    f x = x

simplify :: [Stmt] -> [Stmt]
simplify = map simplifyStmt