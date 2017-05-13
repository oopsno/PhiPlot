module Language.PhiPlot.Desugar ( desugar, desugarAll ) where

import Language.PhiPlot.AST

desugarAll :: [AST] -> [AST]
desugarAll = concatMap desugar 

desugar :: AST -> [AST]
desugar (Def fn args body)             = [ Def   fn      args            $ desugarAll body ]
desugar (For var start stop step body) = [ For   var     start stop step $ desugarAll body ]
desugar (If cond t f)                  = [ If    cond  ( desugarAll t )  ( desugarAll f )  ]
desugar (While cond body)              = [ While cond                    $ desugarAll body ]
desugar Void                           = [ ]
desugar ast                            = [ ast ]