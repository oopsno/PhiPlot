{-# LANGUAGE LambdaCase #-}

module Language.PhiPlot.Lexer where

import qualified Control.Monad (void)
import Data.Char (toLower)
import Text.Parsec (oneOf, try, (<|>))
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T

ops :: [String]
ops =
  [ "+",
    "*",
    "**",
    "-",
    "/",
    "<",
    ">",
    "<=",
    ">=",
    "==",
    "!=",
    "=",
    "&&",
    "||",
    ",",
    ";"
  ]

names :: [String]
names =
  [ "def",
    "return",
    "if",
    "else",
    "for",
    "from",
    "to",
    "step",
    "break",
    "is",
    "true",
    "false"
  ]

lexer :: T.TokenParser ()
lexer =
  T.makeTokenParser $
    emptyDef
      { T.commentStart = "/*",
        T.commentEnd = "*/",
        T.commentLine = "//",
        T.nestedComments = True,
        T.reservedOpNames = ops,
        T.reservedNames = names,
        T.caseSensitive = False
      }

-- NOTE: (T.naturalOrFloat lexer) :: Either Integer Double
number :: Parser Double
number = either fromInteger id <$> T.naturalOrFloat lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

braces :: Parser a -> Parser a
braces = T.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = T.semiSep lexer

-- PhiPlot is NOT case sensitive
identifier :: Parser String
identifier = map toLower <$> T.identifier lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer
