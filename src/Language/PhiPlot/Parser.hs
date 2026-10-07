module Language.PhiPlot.Parser (parseSource, parseFile) where

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
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T
import Text.Printf (PrintfArg (parseFormat))
import Prelude hiding (Ordering (..))

-- Parse arithmetical expressions

arithAtom :: Parser Expr
arithAtom =
  Imm <$> number
    <|> try call
    <|> try variable
    <|> try (parens arithExpr)
    <?> "athithAtom"

arithExpr :: Parser Expr
arithExpr = E.buildExpressionParser table arithAtom <?> "arithmetical expression"
  where
    prefixs = map $ \(s, f) -> E.Prefix (reservedOp s >> return (UniOp f))
    infixls = map $ \(s, f) -> E.Infix (reservedOp s >> return (BinOp f)) E.AssocLeft
    table =
      [ prefixs [("+", Positive), ("-", Negative)],
        infixls [("**", Pow)],
        infixls [("*", Mul), ("/", Div)],
        infixls [("+", Plus), ("-", Minus)]
      ]

arithExprPair :: Parser Expr
arithExprPair = do
  reservedOp "("
  lhs <- arithExpr
  reservedOp ","
  rhs <- arithExpr
  reservedOp ")"
  pure $ Pair lhs rhs

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep arithExpr
  return $ Call name args

variable :: Parser Expr
variable = Var <$> identifier

-- Parser logic expressions

boolAtom :: Parser BoolExpr
boolAtom =
  (BoolAtom True <$ reserved "true")
    <|> (BoolAtom False <$ reserved "false")
    <|> try compareExpr
    <|> parens boolExpr
    <|> nonzero
    <?> "boolAtom"
  where
    nonzero :: Parser BoolExpr
    nonzero = Nonzero <$> arithExpr

compareExpr :: Parser BoolExpr
compareExpr = do
  lhs <- arithExpr
  op <- cmpOp
  rhs <- arithExpr
  pure $ Cmp op lhs rhs

cmpOp :: Parser CompareOperator
cmpOp =
  (EQ <$ reservedOp "==")
    <|> (NE <$ reservedOp "!=")
    <|> (LE <$ reservedOp "<=")
    <|> (GE <$ reservedOp ">=")
    <|> (LT <$ reservedOp "<")
    <|> (GT <$ reservedOp ">")
    <?> "CompareOperator"

boolExpr :: Parser BoolExpr
boolExpr = E.buildExpressionParser table boolAtom <?> "boolean expression"
  where
    table =
      [ [E.Prefix (Not <$ reservedOp "!")],
        [E.Infix (LogicOp AND <$ reservedOp "&&") E.AssocLeft],
        [E.Infix (LogicOp OR <$ reservedOp "||") E.AssocLeft]
      ]

-- Statement

stmt :: Parser Stmt
stmt =
  try cond
    <|> try block
    <|> try breakStmt
    <|> try returnStmt
    <|> try for
    <|> try assign
    <|> try astmt

block :: Parser Stmt
block = Block <$> braces (many stmt)

astmt :: Parser Stmt
astmt = AExp <$> arithExpr <* semicolon

bstmt :: Parser Stmt
bstmt = BExp <$> boolExpr <* semicolon

assign :: Parser Stmt
assign = do
  k <- identifier
  (reservedOp "=") <|> try (reserved "is") <?> "assign operator"
  v <- try arithExprPair <|> try arithExpr <?> "value to assign"
  semicolon
  pure $ Assign k v

defun :: Parser Stmt
defun = do
  reserved "def"
  name <- identifier
  args <- parens $ commaSep identifier
  Def name args <$> block

returnStmt :: Parser Stmt
returnStmt = do
  reserved "return"
  Return <$> arithExpr <* semicolon

cond :: Parser Stmt
cond = do
  reserved "if"
  cond <- boolExpr
  trueCase <- block
  falseCase <- option Void (reserved "else" >> block)
  return $ If cond trueCase falseCase

for :: Parser Stmt
for = do
  reserved "for"
  var <- identifier
  reserved "from"
  start <- arithExpr
  reserved "to"
  stop <- arithExpr
  step <- option (Imm 1) (reserved "step" >> arithExpr)
  For var start stop step <$> (try block <|> try draw <?> "block stmt or draw (x, y)")
  where
    draw = AExp <$> call <* semicolon

semicolon :: Parser Stmt
semicolon = reservedOp ";" >> return Void

breakStmt :: Parser Stmt
breakStmt = reserved "break" >> semicolon >> return Break

-- The full parser
program :: Parser Module
program = Module <$> many (try stmt <|> try defun)

runParser :: Parser a -> Parser a
runParser p = do
  T.whiteSpace lexer
  r <- p
  eof
  pure r

parseSource :: String -> Either ParseError Module
parseSource = parse (runParser program) "<stdin>"

parseFile :: FilePath -> IO (Either ParseError Module)
parseFile p = do
  code <- readFile p
  pure $ parse (runParser program) p code
