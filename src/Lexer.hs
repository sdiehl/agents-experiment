module Lexer where

import Text.Parsec hiding (State)
import Control.Monad.State

import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Token as Tok

type IParser a = ParsecT String () (State SourcePos) a

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
    "NOT",
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

lexerStyle :: Tok.GenLanguageDef String () (State SourcePos)
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

{-lexer :: Tok.GenTokenParser ()-}
lexer :: Tok.GenTokenParser String () (State SourcePos)
lexer = Tok.makeTokenParser lexerStyle

reservedOp :: String -> IParser ()
reservedOp = Tok.reservedOp lexer

reserved :: String -> IParser ()
reserved = Tok.reserved lexer

identifier :: IParser String
identifier = Tok.identifier lexer

brackets :: IParser a -> IParser a
brackets = Tok.brackets lexer

parens :: IParser a -> IParser a
parens = Tok.parens lexer

commaSep :: IParser a -> IParser [a]
commaSep = Tok.commaSep lexer

semiSep :: IParser a -> IParser [a]
semiSep = Tok.semiSep lexer

semiSep1 :: IParser a -> IParser [a]
semiSep1 = Tok.semiSep1 lexer

semi :: IParser String
semi = Tok.semi lexer

comma :: IParser String
comma = Tok.comma lexer

colon :: IParser String
colon = Tok.colon lexer

braces :: IParser a -> IParser a
braces = Tok.braces lexer

integer :: IParser Integer
integer = Tok.integer lexer

chr :: IParser Char
chr = Tok.charLiteral lexer

str :: IParser String
str = Tok.stringLiteral lexer

contents :: IParser a -> IParser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r
