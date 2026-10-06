module Language.PhiPlot.Simplify (simplify) where

import Data.Generics
import Language.PhiPlot.AST (Stmt (..))

simplifyStmt :: Stmt -> Stmt
simplifyStmt = everywhere (mkT f)
  where
    f :: Stmt -> Stmt
    f (Block []) = Void
    f (Block [Void]) = Void
    f x = x

simplify :: [Stmt] -> [Stmt]
simplify = map simplifyStmt