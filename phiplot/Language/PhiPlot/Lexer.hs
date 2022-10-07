module Language.PhiPlot.Lexer where

import qualified Control.Monad (void)
import Data.Char
import Text.Parsec (oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tk

ops :: [String]
ops = ["+", "*", "**", "-", "/", ";", "<", ">", "<=", ">=", "==", "!=", "="]

names :: [String]
names =
  [ "def",
    "extern",
    "return",
    "if",
    "else",
    "for",
    "is"
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

float :: Parser Double
float = Tk.float lexer

intfloat :: Parser Double
intfloat = fromIntegral <$> Tk.integer lexer

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