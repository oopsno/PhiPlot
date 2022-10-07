module Language.PhiPlot.Parser where

import Data.Functor
import Language.PhiPlot.AST
import Language.PhiPlot.Lexer
import Text.Parsec
    ( ParseError, option, eof, (<?>), (<|>), many, parse, try )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tk
import qualified Text.Parsec.Token as Tok
import Text.Printf (PrintfArg (parseFormat))
import Prelude hiding (Ordering (..))

-- Parse algebra expressions
aexpr :: Parser Expr
aexpr = Ex.buildExpressionParser table factor <?> "aexpr"
  where
    prefixs = map $ \(s, f) -> Ex.Prefix (reservedOp s >> return (UniOp f))
    infixls = map $ \(s, f) -> Ex.Infix (reservedOp s >> return (BinOp f)) Ex.AssocLeft
    table =
      [ prefixs [("+", Positive), ("-", Negative)],
        infixls [("**", Pow)],
        infixls [("*", Mul), ("/", Div)],
        infixls [("+", Plus), ("-", Minus)]
      ]
    factor =
      try immediate
        <|> try pair
        <|> try call
        <|> try variable
        <|> try (parens aexpr)
        <?> "factor"

-- Parser logic expressions

bexpr :: Parser BoolExpr
bexpr = Ex.buildExpressionParser table factor <?> "boolean expression"
  where
    table =
      [ [Ex.Prefix (reservedOp "!" >> return Not)],
        map cmp [("<", LT), (">", GT), ("<=", LE), (">=", GE), ("==", EQ), ("!=", NE)],
        map makeBoolExpr [("&&", AND), ("||", OR)]
      ]
    cmp (s, f) = Ex.Infix (reservedOp s >> return (Cmp f)) Ex.AssocLeft
    makeBoolExpr (s, f) = Ex.Infix (reservedOp s >> return (LogicOp f)) Ex.AssocLeft
    factor = try nonzero <|> parens bexpr

-- Componentions

immediate :: Parser Expr
immediate = Imm <$> (try float <|> try intfloat)

variable :: Parser Expr
variable = Var <$> identifier

pair :: Parser Expr
pair = do
  reserved "("
  x <- aexpr
  reserved ","
  y <- aexpr
  reserved ")"
  return $ Pair x y

nonzero :: Parser BoolExpr
nonzero = Nonzero <$> aexpr

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

-- Statement
stmt :: Parser Stmt
stmt =
  try cond
    <|> try block
    <|> try for
    <|> try assign
    <|> try bstmt
    <|> try astmt
    <|> try semi
    <|> try returnStmt
    <|> try breakStmt

block :: Parser Stmt
block = Block <$> braces (many stmt)

astmt :: Parser Stmt
astmt = AExp <$> aexpr

bstmt :: Parser Stmt
bstmt = BExp <$> bexpr

assign :: Parser Stmt
assign = do
  k <- identifier
  try (reserved "is") <|> try (reservedOp "=") <?> "use = or is to assign"
  Assign k <$> aexpr

defun :: Parser Stmt
defun = do
  reserved "def"
  name <- identifier
  args <- parens $ commaSep variable
  Def name args <$> block

returnStmt :: Parser Stmt
returnStmt = reserved "return" >> Return <$> aexpr

cond :: Parser Stmt
cond = do
  reserved "if"
  cond <- bexpr
  trueCase <- block
  falseCase <- option Void (reserved "else" >> block)
  return $ If cond trueCase falseCase

for :: Parser Stmt
for = do
  reserved "for"
  var <- identifier
  reserved "from"
  start <- aexpr
  reserved "to"
  stop <- aexpr
  step <- option (Imm 1) (reserved "step" >> aexpr)
  For var start stop step <$> (try block <|> try draw <?> "block stmt or draw (x, y)")
  where
    draw = AExp <$> call

semi :: Parser Stmt
semi = reservedOp ";" >> return Void

breakStmt :: Parser Stmt
breakStmt = reserved "break" >> return Break

-- The full parser

toplevel :: Parser [Stmt]
toplevel = many $ try stmt <|> try defun

parsePhiplot :: String -> Either ParseError [Stmt]
parsePhiplot = parse (contents toplevel) "<stdin>"