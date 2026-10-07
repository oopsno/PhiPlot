{-# LANGUAGE LambdaCase #-}

module Language.PhiPlot.Lexer where

import qualified Control.Monad (void)
import Data.Char
import Data.Functor ((<&>))
import Text.Parsec (oneOf, try, (<|>))
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tk

ops :: [String]
ops = ["+", "*", "**", "-", "/", ";", "<", ">", "<=", ">=", "==", "!=", "="]

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

lexer :: Tk.TokenParser ()
lexer =
  Tk.makeTokenParser $
    emptyDef
      { Tk.commentStart = "/*",
        Tk.commentEnd = "*/",
        Tk.commentLine = "//",
        Tk.nestedComments = True,
        Tk.reservedOpNames = ops,
        Tk.reservedNames = names,
        Tk.caseSensitive = False
      }

number :: Parser Double
number =
  Tk.naturalOrFloat lexer <&> \case
    Left i -> fromInteger i
    Right f -> f

parens :: Parser a -> Parser a
parens = Tk.parens lexer

braces :: Parser a -> Parser a
braces = Tk.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tk.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tk.semiSep lexer

-- PhiPlot is NOT case sensitive
identifier :: Parser String
identifier = map toLower `fmap` Tk.identifier lexer

reserved :: String -> Parser ()
reserved = Tk.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tk.reservedOp lexer

symbol :: String -> Parser ()
symbol s = Control.Monad.void (Tk.symbol lexer s)