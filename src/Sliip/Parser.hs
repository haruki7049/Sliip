-- |
-- Module      : Sliip.Parser
-- Description : Parser for the Sliip Lisp dialect
-- Maintainer  : haruki7049
--
-- This module provides a parser for Sliip, a Lisp dialect with additional features.
-- The parser is built using the Parsec library and supports:
--
-- * Basic Lisp forms: numbers, strings, booleans, symbols
-- * Lambda expressions with optional type annotations
-- * Various binding forms: @let@, @let*@, @letrec@
-- * Conditional expressions: @if@
-- * Sequential execution: @begin@
-- * Quotation: @quote@
-- * Algebraic data types: @def-type@
-- * Pattern matching: @match@
-- * Type ascriptions: @as@
--
-- The parser produces an abstract syntax tree (AST) represented by the 'Expr' type.
module Sliip.Parser
  ( -- * Parser API
    parseSliip,
    Programs,

    -- * AST Types
    Expr (..),
    Param (..),
    TypeExpr (..),
    Ctor (..),
    Pattern (..),
  )
where

import Data.Void (Void)
import Text.Megaparsec
  ( ParseErrorBundle,
    Parsec,
    eof,
    lookAhead,
    many,
    manyTill,
    notFollowedBy,
    oneOf,
    optional,
    parse,
    some,
    try,
    (<|>),
  )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- Extended AST --------------------------------------------------------------

-- | Main expression type for the Sliip AST.
--
-- Represents all forms that can appear in a Sliip program.
data Expr
  = -- | Integer literal
    ENumber Integer
  | -- | Floating-point literal
    EFloat Double
  | -- | String literal
    EString String
  | -- | Boolean literal
    EBool Bool
  | -- | Variable reference
    ESymbol String
  | -- | List literal (currently unused)
    EList [Expr]
  | -- | Top-level definition: @(define name expr)@
    EDefine String Expr
  | -- | Lambda expression: @(lambda (params...) body...)@
    ELambda [Param] [Expr]
  | -- | Conditional: @(if condition then else)@
    EIf Expr Expr Expr
  | -- | Parallel let binding: @(let ((x e)...) body...)@
    ELet [(String, Expr)] [Expr]
  | -- | Sequential let binding: @(let* ((x e)...) body...)@
    ELetStar [(String, Expr)] [Expr]
  | -- | Recursive let binding: @(letrec ((x e)...) body...)@
    ELetRec [(String, Expr)] [Expr]
  | -- | Sequential execution: @(begin expr...)@
    EBegin [Expr]
  | -- | Quotation: @(quote expr)@
    EQuote Expr
  | -- | Type ascription: @(as expr type)@
    EAscription Expr TypeExpr
  | -- | Type definition: @(def-type name (params...) ctors...)@
    EDefType String [String] [Ctor]
  | -- | Pattern matching: @(match expr (pattern body...)...)@
    EMatch Expr [(Pattern, [Expr])]
  | -- | Function application: @(f args...)@
    EApp Expr [Expr]
  deriving (Show, Eq)

-- | Function parameter with optional type annotation.
data Param = Param String (Maybe TypeExpr)
  deriving (Show, Eq)

-- | Type expressions for type annotations.
data TypeExpr
  = -- | Simple type name: @Int@, @String@, etc.
    TName String
  | -- | Type application: @(List Int)@
    TApp String [TypeExpr]
  | -- | Function type: @(-> a b c)@ means @a -> b -> c@
    TArrow [TypeExpr]
  deriving (Show, Eq)

-- | Constructor for algebraic data types.
--
-- Represents a constructor with its name and field types.
data Ctor = Ctor String [TypeExpr]
  deriving (Show, Eq)

-- | Pattern for pattern matching.
data Pattern
  = -- | Variable pattern: binds to a variable
    PVar String
  | -- | Wildcard pattern: @_@
    PWildcard
  | -- | Unit pattern: @()@
    PUnit
  | -- | Constructor pattern: @(Cons x xs)@
    PCtor String [Pattern]
  deriving (Show, Eq)

-- | A program is a sequence of expressions (typically top-level definitions).
type Programs = [Expr]

-- Lexer ---------------------------------------------------------------------

-- | Parser type for Sliip.
type Parser = Parsec Void String

-- | Reserved keywords that cannot be used as identifiers.
reservedWords :: [String]
reservedWords =
  [ "define",
    "lambda",
    "let",
    "let*",
    "letrec",
    "if",
    "begin",
    "quote",
    "def-type",
    "match",
    "as",
    "true",
    "false",
    "Nil",
    "Cons"
  ]

-- | Parse whitespace and comments.
sc :: Parser ()
sc = L.space C.space1 lineComment blockComment
  where
    lineComment = L.skipLineComment ";"
    blockComment = L.skipBlockCommentNested "#|" "|#"

-- | Lexeme parser: parse something and skip trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a specific string (symbol) and skip trailing whitespace.
symbol' :: String -> Parser String
symbol' = L.symbol sc

-- | Parse something enclosed in parentheses.
parens' :: Parser a -> Parser a
parens' = between (symbol' "(") (symbol' ")")
  where
    between open close p = open *> p <* close

-- | Parse whitespace and comments.
whiteSpace' :: Parser ()
whiteSpace' = sc

-- | Parse an identifier.
identifier' :: Parser String
identifier' = lexeme (try $ do
  name <- (:) <$> identStart <*> many identLetter
  if name `elem` reservedWords
    then fail $ "keyword " ++ name ++ " cannot be an identifier"
    else return name)
  where
    identStart = C.letterChar <|> oneOf "+-*/<>=!?_"
    identLetter = C.alphaNumChar <|> oneOf "+-*/<>=!?_-'"

-- | Parse a reserved keyword.
reserved' :: String -> Parser ()
reserved' w = lexeme $ try $ do
  _ <- C.string w
  notFollowedBy (C.alphaNumChar <|> oneOf "+-*/<>=!?_-'")

-- | Parse a reserved operator.
reservedOp' :: String -> Parser ()
reservedOp' = reserved'

-- | Parse a string literal.
stringLiteral' :: Parser String
stringLiteral' = lexeme $ C.char '"' >> manyTill L.charLiteral (C.char '"')

-- | Parse a number (integer or float).
naturalOrFloat' :: Parser (Either Integer Double)
naturalOrFloat' = lexeme $ try float <|> integer
  where
    integer = Left <$> L.decimal
    float = Right <$> L.float

-- Basic parsers --------------------------------------------------------------

-- | Parse a numeric literal (integer or float).
parseNumber :: Parser Expr
parseNumber = do
  nf <- naturalOrFloat'
  return $ case nf of
    Left i -> ENumber i
    Right d -> EFloat d

-- | Parse a string literal.
parseString :: Parser Expr
parseString = EString <$> stringLiteral'

-- | Parse a boolean literal (@true@ or @false@).
parseBool :: Parser Expr
parseBool =
  (reserved' "true" >> return (EBool True))
    <|> (reserved' "false" >> return (EBool False))

-- | Parse a symbol (variable reference).
parseSymbol :: Parser Expr
parseSymbol = ESymbol <$> identifier'

-- Type parser ---------------------------------------------------------------

-- | Parse a type expression.
--
-- Can be either an arrow type or an atomic type.
parseType :: Parser TypeExpr
parseType = try parseArrow <|> parseTypeAtom

-- | Parse an atomic type (type name or type application).
parseTypeAtom :: Parser TypeExpr
parseTypeAtom =
  parens'
    ( do
        name <- identifier'
        args <- many parseType
        return $ TApp name args
    )
    <|> (TName <$> identifier')

-- | Parse a function type: @(-> a b c)@.
parseArrow :: Parser TypeExpr
parseArrow = parens' $ do
  _ <- symbol' "->"
  ts <- some parseType
  return $ TArrow ts

-- Pattern parser ------------------------------------------------------------

-- | Parse a pattern for pattern matching.
--
-- Supports:
--
-- * Wildcard: @_@
-- * Unit: @()@
-- * Constructor: @(Cons x xs)@
-- * Variable: @x@
parsePattern :: Parser Pattern
parsePattern =
  (reservedOp' "_" >> return PWildcard)
    <|> parens'
      ( do
          name <- identifier'
          pats <- many parsePattern
          return $ PCtor name pats
      )
    <|> try (C.string "()" >> return PUnit)
    <|> (PVar <$> identifier')

-- Constructors (def-type) --------------------------------------------------

-- | Parse a data type constructor.
--
-- Format: @(ConstructorName Type1 Type2 ...)@
parseCtor :: Parser Ctor
parseCtor = parens' $ do
  name <- identifier'
  tys <- many parseType
  return $ Ctor name tys

-- Expressions ---------------------------------------------------------------

-- | Parse a type ascription: @(as expr type)@.
parseAscription :: Parser Expr
parseAscription = do
  reserved' "as"
  e <- parseExpr
  EAscription e <$> parseType

-- | Parse a top-level definition: @(define name expr)@.
parseDefine :: Parser Expr
parseDefine = do
  reserved' "define"
  name <- identifier'
  EDefine name <$> parseExpr

-- | Parse a lambda expression: @(lambda (params...) body...)@.
parseLambda :: Parser Expr
parseLambda = do
  reserved' "lambda"
  params <- parens' (many parseParam)
  body <- some parseExpr
  return $ ELambda params body

-- | Parse a lambda parameter, optionally with a type annotation.
--
-- * @x@ - simple parameter
-- * @(x Type)@ - parameter with type annotation
parseParam :: Parser Param
parseParam =
  try
    ( parens' $ do
        n <- identifier'
        Param n . Just <$> parseType
    )
    <|> (Param <$> identifier' <*> pure Nothing)

-- | Parse let-like binding forms (@let@, @let*@, @letrec@).
--
-- Format: @(let ((var expr)...) body...)@
parseLetLike :: String -> Parser Expr
parseLetLike kw = do
  reserved' kw
  binds <- parens' (many parseBinding)
  body <- some parseExpr
  case kw of
    "let" -> return $ ELet binds body
    "let*" -> return $ ELetStar binds body
    "letrec" -> return $ ELetRec binds body
    _ -> fail "unknown let-like"

-- | Parse a binding in a let form.
--
-- Can be either:
--
-- * @(var expr)@ - simple binding
-- * @(var type expr)@ - binding with type annotation
parseBinding :: Parser (String, Expr)
parseBinding = parens' $ try bindingWithType <|> bindingSimple
  where
    bindingSimple = do
      name <- identifier'
      expr <- parseExpr
      return (name, expr)
    bindingWithType = do
      name <- identifier'
      -- try parse a type next; if it fails, backtrack and parse as expr
      t <- try parseType
      initExpr <- parseExpr
      return (name, EAscription initExpr t)

-- | Parse an if expression: @(if condition then else)@.
parseIf :: Parser Expr
parseIf = do
  reserved' "if"
  c <- parseExpr
  t <- parseExpr
  EIf c t <$> parseExpr

-- | Parse a begin expression: @(begin expr...)@.
--
-- Evaluates expressions in sequence, returning the value of the last one.
parseBegin :: Parser Expr
parseBegin = do
  reserved' "begin"
  es <- some parseExpr
  return $ EBegin es

-- | Parse a quote expression: @(quote expr)@.
parseQuote :: Parser Expr
parseQuote = do
  reserved' "quote"
  EQuote <$> parseExpr

-- | Parse a type definition: @(def-type Name (params...) constructors...)@.
--
-- Example: @(def-type List (a) (Nil) (Cons a (List a)))@
parseDefType :: Parser Expr
parseDefType = do
  reserved' "def-type"
  name <- identifier'
  params <- parens' (many identifier')
  ctors <- some parseCtor
  return $ EDefType name params ctors

-- | Parse a pattern matching expression: @(match expr (pattern body...)...)@.
parseMatch :: Parser Expr
parseMatch = do
  reserved' "match"
  expr <- parseExpr
  clauses <-
    some
      ( parens' $ do
          pat <- parsePattern
          body <- some parseExpr
          return (pat, body)
      )
  return $ EMatch expr clauses

-- Generic list/app parsing --------------------------------------------------

-- | Parse a parenthesized expression (special form or application).
--
-- Uses lookahead to determine which special form to parse, or falls back
-- to parsing as a function application.
parseApplicationOrSpecial :: Parser Expr
parseApplicationOrSpecial = parens' $ do
  whiteSpace'
  look <- optional (lookAhead (some (C.letterChar <|> oneOf "+-*/<>=!?_-'")))
  case look of
    Just "define" -> parseDefine
    Just "lambda" -> parseLambda
    Just "let" -> parseLetLike "let"
    Just "let*" -> parseLetLike "let*"
    Just "letrec" -> parseLetLike "letrec"
    Just "if" -> parseIf
    Just "begin" -> parseBegin
    Just "quote" -> parseQuote
    Just "as" -> parseAscription
    Just "def-type" -> parseDefType
    Just "match" -> parseMatch
    _ -> do
      -- not one of the special forms: parse as application/list
      es <- many parseExpr
      case es of
        [] -> return $ EList []
        (f : args) -> return $ EApp f args

-- Top-level expression parser (includes atom parsers) ----------------------

-- | Parse an atomic expression (literal or symbol).
parseAtom :: Parser Expr
parseAtom =
  try parseNumber
    <|> try parseString
    <|> try parseBool
    <|> try (parens' (do _ <- C.char '\''; EQuote <$> parseExpr)) -- '( ...) rare
    <|> parseSymbol

-- | Parse any expression.
--
-- This is the main entry point for expression parsing.
parseExpr :: Parser Expr
parseExpr =
  whiteSpace'
    *> ( try parseAtom
           <|> try parseApplicationOrSpecial
       )

-- Program -------------------------------------------------------------------

-- | Parse a complete Sliip program.
--
-- A program consists of zero or more expressions (typically definitions and other top-level forms).
parseProgram :: Parser Programs
parseProgram = whiteSpace' *> many parseExpr <* eof

-- Public API ----------------------------------------------------------------

-- | Parse a Sliip program from a string.
--
-- Returns either a parse error or the parsed program (list of expressions).
parseSliip :: String -> Either (ParseErrorBundle String Void) Programs
parseSliip = parse parseProgram ""
