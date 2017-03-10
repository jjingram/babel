module Lexer where

import Text.Parsec ((<|>), oneOf)
import Text.Parsec.Char (letter, digit)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@^_~"

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef { Token.commentStart = "#|"
                     , Token.commentEnd = "|#"
                     , Token.commentLine = ";"
                     , Token.identStart = letter <|> symbol
                     , Token.identLetter = letter <|> symbol <|> digit
                     , Token.reservedNames = [ "quote"
                                             , "begin"
                                             , "def"
                                             , "if"
                                             , "lambda"
                                             , "ext"
                                             ]
                     }

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
