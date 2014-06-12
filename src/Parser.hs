module Parser (
  parseFile,
) where

import Text.Show.Pretty
import Control.Monad.State

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import qualified Text.Parsec.Expr as Ex

import AST
import Lexer

-------------------------------------------------------------------------------
-- Condtional
-------------------------------------------------------------------------------

type Op = Ex.Operator String () (State SourcePos)

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

prefixOp :: String -> (a -> a) -> Op a
prefixOp x f = Ex.Prefix (reservedOp x >> return f)

operators :: [[Op Cond]]
operators = [
    -- infix comparison
    [
      infixOp ">"  Gt Ex.AssocLeft
    , infixOp "<"  Lt Ex.AssocLeft
    , infixOp ">=" Gte Ex.AssocLeft
    , infixOp "<=" Lte Ex.AssocLeft
    , infixOp "IS" Eq Ex.AssocLeft
    ],

    -- infix logical
    [
      infixOp "AND" And Ex.AssocLeft,
      infixOp "OR" Or Ex.AssocLeft
    ]
  ]

cexpr :: IParser Cond
cexpr =  Ex.buildExpressionParser operators cfactor

prop :: IParser Cond
prop = do
  nm <- identifier
  return $ Var nm

num :: IParser Cond
num = do
  nm <- integer
  return $ Const nm

has :: IParser Cond
has = do
  reserved "HAS"
  x <- identifier
  return $ Has (Tag x)

neg :: IParser Cond
neg = do
  reserved "NOT"
  x <- cfactor
  return $ Not x

cfactor :: IParser Cond
cfactor
   =  num
  <|> has
  <|> prop
  <|> neg
  <|> parens cexpr

-------------------------------------------------------------------------------
-- Object
-------------------------------------------------------------------------------

obj :: IParser Object
obj = do
  o <- withBlock mkObj objname (measuredef <|> actuatordef)
  spaces
  return $ o

objname :: IParser String
objname = do
  reserved "OBJECT"
  identifier

-------------------------------------------------------------------------------
-- Measure
-------------------------------------------------------------------------------

measuredef :: IParser ObjDecl
measuredef = do
  reserved "MEASURE"
  name <- identifier
  val <- optionMaybe $ do
    reservedOp "="
    n <- integer
    return n
  spaces
  return $ M $ Measure name (maybe 0 id val)

-------------------------------------------------------------------------------
-- Actuator
-------------------------------------------------------------------------------

aif :: IParser Cond
aif = do
  reserved "IF"
  cond <- cexpr
  colon
  return cond

aalways :: IParser Cond
aalways = do
  reserved "ALWAYS"
  colon
  return Tr

mkActuator :: String -> Cond -> [Action] -> ObjDecl
mkActuator "if" cond acts = A $ Actuator (If cond) acts
mkActuator "always" _ acts = A $ Actuator Always acts

actuatordef :: IParser ObjDecl
actuatordef =
      withBlock (mkActuator "if") aif actions
  <|> withBlock (mkActuator "always") aalways actions

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

actions :: IParser Action
actions =
      noop
  <|> create
  <|> zero
  <|> inc
  <|> dec
  <|> destroy
  <|> tag
  <|> untag

create :: IParser Action
create = do
  reserved "CREATE"
  nm <- identifier
  return $ Create nm

destroy :: IParser Action
destroy = do
  reserved "DESTROY"
  nm <- identifier
  -- XXX
  return $ Destroy Self

inc :: IParser Action
inc = do
  reserved "INC"
  nm <- identifier
  val <- optionMaybe $ do
    reserved "BY"
    integer
  return $ Inc nm (maybe 1 id val)

dec :: IParser Action
dec = do
  reserved "DEC"
  nm <- identifier
  val <- optionMaybe $ do
    reserved "BY"
    integer
  return $ Dec nm (maybe 1 id val)

zero :: IParser Action
zero = do
  reserved "ZERO"
  nm <- identifier
  return $ Zero nm

tag :: IParser Action
tag = do
  reserved "TAG"
  nm <- identifier
  return $ Set (Tag nm)

untag :: IParser Action
untag = do
  reserved "UNTAG"
  nm <- identifier
  return $ Unset (Tag nm)

noop :: IParser Action
noop = do
  reserved "NOOP"
  return Noop

-------------------------------------------------------------------------------
-- World
-------------------------------------------------------------------------------

entity :: IParser (Int, String)
entity = do
  val <- optionMaybe integer
  nm <- identifier
  return $ ((maybe 0 fromIntegral val), nm)

world :: IParser Decl
world = do
  reserved "WORLD"
  nm <- identifier
  reserved "IS"
  entities <- commaSep entity
  return $ WorldDecl nm entities

objdecl :: IParser Decl
objdecl = do
  o <- obj
  return $ ObjectDecl o

probe :: IParser Decl
probe = do
  path <- identifier `sepBy1` (char '.')
  label <- optionMaybe $ do
    reserved "as"
    identifier
  return $ ProbeDecl (Probe path) label

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

decl :: IParser Decl
decl =  objdecl
    <|> world
    <|> probe

decls :: IParser [Decl]
decls = many decl

modl :: IParser Module
modl = do
  ds <- decls
  return $ Module "" ds

parser :: IParser a -> SourceName -> String -> Either ParseError a
parser f source_name input = runIndent source_name $
  runParserT f () source_name input

parseFile :: FilePath -> IO (Either ParseError Module)
parseFile fname = do
  fcontents <- readFile fname
  return $ parser (contents modl) fname fcontents
