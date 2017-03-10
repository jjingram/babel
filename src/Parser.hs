module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token
import Text.Parsec.Expr

import qualified Lexer
import Syntax

integer :: Parser Expr
integer = do
  n <- Lexer.integer
  return $ Float (fromInteger n)

float :: Parser Expr
float = do
  n <- Lexer.float
  return $ Float n

symbol :: Parser Expr
symbol = do
  sym <- Lexer.identifier
  return $ Var sym

defun :: Parser Expr
defun = do
  Lexer.reserved "defun"
  name <- Lexer.identifier
  params <- Lexer.parens (many Lexer.identifier)
  body <- expr
  return $ Function name params body

extern :: Parser Expr
extern = do
  Lexer.reserved "ext"
  name <- Lexer.identifier
  args <- many Lexer.identifier
  return $ Extern name args

call :: Parser Expr
call = do
  name <- Lexer.identifier
  args <- many expr
  return $ Call name args

expr :: Parser Expr
expr = try Parser.integer
   <|> try Parser.float
   <|> try Parser.symbol
   <|> Lexer.parens (try defun <|> try extern <|> call)

contents :: Parser a -> Parser a
contents p = do
  whiteSpace Lexer.lexer
  res <- p
  eof
  return res

toplevel :: Parser [Expr]
toplevel = many expr

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = Text.Parsec.parse (contents toplevel) "<stdin>" s
