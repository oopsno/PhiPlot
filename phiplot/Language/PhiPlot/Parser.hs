module Language.PhiPlot.Parser (parsePhiplot) where

import Control.Monad (liftM2)
import Data.Functor
import Language.PhiPlot.AST
import Language.PhiPlot.Lexer
  ( braces,
    commaSep,
    identifier,
    lexer,
    number,
    parens,
    reserved,
    reservedOp,
  )
import Text.Parsec
  ( ParseError,
    char,
    eof,
    many,
    option,
    parse,
    try,
    (<?>),
    (<|>),
  )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tk
import qualified Text.Parsec.Token as Tok
import Text.Printf (PrintfArg (parseFormat))
import Prelude hiding (Ordering (..))

-- Parse algebra expressions
aexpr :: Parser Expr
aexpr = Ex.buildExpressionParser table term <?> "algebra expression"
  where
    prefixs = map $ \(s, f) -> Ex.Prefix (reservedOp s >> return (UniOp f))
    infixls = map $ \(s, f) -> Ex.Infix (reservedOp s >> return (BinOp f)) Ex.AssocLeft
    table =
      [ prefixs [("+", Positive), ("-", Negative)],
        infixls [("**", Pow)],
        infixls [("*", Mul), ("/", Div)],
        infixls [("+", Plus), ("-", Minus)]
      ]
    term =
      try immediate
        <|> try call
        <|> try variable
        <|> try (parens aexpr)
        <?> "algebra term"

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
immediate = Imm <$> number

variable :: Parser Expr
variable = Var <$> identifier

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
    <|> try returnStmt
    <|> try breakStmt
    <|> try setRot
    <|> try setScale
    <|> try setOrigin

block :: Parser Stmt
block = Block <$> braces (many stmt)

astmt :: Parser Stmt
astmt = AExp <$> aexpr <* semicolon

bstmt :: Parser Stmt
bstmt = BExp <$> bexpr <* semicolon

assign :: Parser Stmt
assign = do
  k <- identifier
  try (reserved "is") <|> try (reservedOp "=") <?> "use = or is to assign"
  Assign k <$> aexpr <* semicolon

defun :: Parser Stmt
defun = do
  reserved "def"
  name <- identifier
  args <- parens $ commaSep variable
  Def name args <$> block

returnStmt :: Parser Stmt
returnStmt = do
  reserved "return"
  Return <$> aexpr <* semicolon

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
    draw = AExp <$> call <* semicolon

semicolon :: Parser Stmt
semicolon = reservedOp ";" >> return Void

breakStmt :: Parser Stmt
breakStmt = reserved "break" >> semicolon >> return Break

xCommaY = do
  x <- aexpr
  reservedOp ","
  y <- aexpr
  return (x, y)

setOrigin = do
  reserved "origin"
  reserved "is"
  (x, y) <- parens xCommaY
  SetOrigin x y <$ semicolon

setScale = do
  reserved "scale"
  reserved "is"
  (x, y) <- parens xCommaY
  SetScale x y <$ semicolon

setRot = do
  reserved "rot"
  reserved "is"
  SetRot <$> aexpr <* semicolon

-- The full parser
toplevel :: Parser [Stmt]
toplevel = many $ try stmt <|> try defun

parsePhiplot :: String -> Either ParseError [Stmt]
parsePhiplot = parse (contents toplevel) "<stdin>"