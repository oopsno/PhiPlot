module Language.PhiPlot.Parser where

import Prelude hiding (Ordering(..))

import Control.Monad ( liftM, liftM2, liftM3, liftM3, liftM4, liftM5 )

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Language.PhiPlot.Lexer
import Language.PhiPlot.AST

-- Parse algebra expressions

binopl (s, f) = Ex.Infix (reservedOp s >> return (BinOp f)) Ex.AssocLeft

prefix (s, f) = Ex.Prefix (reservedOp s >> return (UniOp f))

algops = [ prefixs [ ("+", Positive), ("-", Negative) ] 
         , infixls [ ("*", Mul), ("/", Div) ]
         , infixls [ ("+", Plus), ("-", Minus) ] ]
         where infixls = map binopl
               prefixs = map prefix

aexpr :: Parser Expr
aexpr = Ex.buildExpressionParser algops afactor

afactor :: Parser Expr
afactor = try number
  <|> try call
  <|> variable
  <|> parens aexpr

-- Parser logic expressions

cmp (s, f) = Ex.Infix (reservedOp s >> return (Cmp f)) Ex.AssocLeft
bop (s, f) = Ex.Infix (reservedOp s >> return (LogicOp f)) Ex.AssocLeft

logops = [ [ Ex.Prefix (reservedOp "!" >> return Not) ] 
         , map cmp [ ("<", LT), (">", GT), ("<=", LE)
                   , (">=", GE), ("==", EQ), ("!=", NE) ]
         , map bop [ ("&&", AND), ("||", OR) ] ]

bexpr :: Parser BoolExpr
bexpr = Ex.buildExpressionParser logops bfactor

bfactor :: Parser BoolExpr
bfactor = try beatom <|> try nonzero <|> parens bexpr

beatom :: Parser BoolExpr
beatom = aexpr >>= return . BEAtom

-- Componentions

number :: Parser Expr
number = try numberf <|> try numberi
  where numberf = float >>= return . Number
        numberi = intfloat >>= return . Number

variable :: Parser Expr
variable = identifier >>= return . Var

nonzero :: Parser BoolExpr
nonzero = (try number <|> try variable) >>= return . Nonzero
 
call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep aexpr
  return $ Call name args

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

-- Statements
stmt :: Parser AST
stmt = do
  v <-  try cond
    <|> try for
    <|> try while
    <|> try assign
    <|> try bstmt
    <|> try astmt
    <|> try ret_stmt
  optional $ reservedOp ";"
  return v

block :: Parser [AST]
block = try (braces $ many stmt) <|> try singleStmt
  where singleStmt = do
          s <- stmt
          return [s]

astmt :: Parser AST
astmt = aexpr >>= return . AExp

bstmt :: Parser AST
bstmt = bexpr >>= return . BExp

assign :: Parser AST
assign = do
  k <- identifier
  reservedOp "="
  v <- aexpr
  return $ Assign k v

defun :: Parser AST
defun = reserved "def" >> liftM3 Def identifier args body
  where args = parens $ commaSep variable
        body = braces $ many stmt

ret_stmt :: Parser AST
ret_stmt = reserved "return" >> liftM Return aexpr

cond :: Parser AST
cond = reserved "if" >> liftM3 If bexpr trueCase falseCase
  where trueCase  = (optional $ reserved "then")   >> block
        falseCase = option [Void] (reserved "else" >> block)

for :: Parser AST
for = reserved "for" >> liftM5 For identifier start stop step block
  where start = reserved "from" >> aexpr
        stop  = reserved "to" >> aexpr
        step  = option (Number 1) (reserved "step" >> aexpr)

while :: Parser AST
while = reserved "while" >> liftM2 While bexpr block

-- The full parser

toplevel :: Parser [AST]
toplevel = many $ try stmt <|> try defun

parseAExpr :: String -> Either ParseError Expr
parseAExpr s = parse (contents aexpr) "<stdin>" s

parseBExpr :: String -> Either ParseError BoolExpr
parseBExpr s = parse (contents bexpr) "<stdin>" s

parseToplevel :: String -> Either ParseError [AST]
parseToplevel s = parse (contents toplevel) "<stdin>" s

parsePhiplot :: String -> Either ParseError [AST]
parsePhiplot = parseToplevel
