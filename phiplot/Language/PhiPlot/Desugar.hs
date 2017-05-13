module Language.PhiPlot.Desugar ( desugar, desugarAll ) where

import Language.PhiPlot.AST

import Prelude hiding ( LT )

desugarAll :: [AST] -> [AST]
desugarAll = concatMap desugar 

desugar :: AST -> [AST]
desugar (Def fn args body)             = [ Def   fn      args            $ desugarAll body ]
desugar (If cond t f)                  = [ If    cond  ( desugarAll t )  ( desugarAll f )  ]
desugar (While cond body)              = [ While cond                    $ desugarAll body ]
desugar (For var start stop step body) = [ Assign var start
                                         , While (Cmp LT (BEAtom (Var var)) (BEAtom stop)) $ desugarAll body ++ [Assign var (BinOp Plus (Var var) step)] ]
desugar Void                           = [ ]
desugar ast                            = [ ast ]