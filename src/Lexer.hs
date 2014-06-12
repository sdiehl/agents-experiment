module Lexer where

import Text.Parsec hiding (State)
import Control.Monad.State

import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Token as Tok

type Parser   = ParsecT String () (State SourcePos)
type Lexer    = Tok.GenTokenParser String () (State SourcePos)
type Language = Tok.GenLanguageDef String () (State SourcePos)

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

reservedOps :: [String]
reservedOps = [
    "->",
    "\\",
    ";",
    "+",
    "*",
    "-",
    "=",
    ".",
    "#",
    "<",
    ">",
    "<=",
    ">="
  ]

reservedNames :: [String]
reservedNames = [
    "OBJECT",
    "MEASURE",
    "NOOP",
    "IF",
    "ALWAYS",
    "HAS",
    "NOT",
    "DEC",
    "INC",
    "BY",
    "TAG",
    "UNTAG",
    "as"
  ]

lexerStyle :: Language
lexerStyle = haskellStyle
  { Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "#"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "#'_"
  , Tok.opStart         = Tok.opLetter lexerStyle
  , Tok.opLetter        = oneOf "`~!@$%^&*-+=;:<>./?#"
  , Tok.reservedOpNames = reservedOps
  , Tok.reservedNames   = reservedNames
  , Tok.caseSensitive   = True
  }

lexer :: Lexer
lexer = Tok.makeTokenParser lexerStyle

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

identifier :: Parser String
identifier = Tok.identifier lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Tok.semiSep1 lexer

semi :: Parser String
semi = Tok.semi lexer

comma :: Parser String
comma = Tok.comma lexer

colon :: Parser String
colon = Tok.colon lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

integer :: Parser Integer
integer = Tok.integer lexer

chr :: Parser Char
chr = Tok.charLiteral lexer

str :: Parser String
str = Tok.stringLiteral lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
