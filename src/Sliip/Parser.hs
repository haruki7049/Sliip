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
    parse,
    Programs,

    -- * AST Types
    Expr (..),
    Param (..),
    TypeExpr (..),
    Ctor (..),
    Pattern (..),
  )
where

import Text.Parsec
  ( ParseError,
    alphaNum,
    char,
    eof,
    letter,
    lookAhead,
    many,
    many1,
    oneOf,
    optionMaybe,
    string,
    try,
    (<|>),
  )
import qualified Text.Parsec as TP (parse)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token
  ( LanguageDef,
    TokenParser,
    commentEnd,
    commentLine,
    commentStart,
    identLetter,
    identStart,
    identifier,
    makeTokenParser,
    naturalOrFloat,
    nestedComments,
    parens,
    reserved,
    reservedNames,
    reservedOp,
    stringLiteral,
    symbol,
    whiteSpace,
  )

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

-- | Language definition for the Sliip lexer.
--
-- Defines:
--
-- * Comment syntax (line comments with @;@ and block comments with @#|...|#@)
-- * Valid identifier characters
-- * Reserved keywords
languageDef :: LanguageDef ()
languageDef =
  emptyDef
    { commentLine = ";",
      commentStart = "#|",
      commentEnd = "|#",
      nestedComments = True,
      identStart = letter <|> oneOf "+-*/<>=!?_",
      identLetter = alphaNum <|> oneOf "+-*/<>=!?_-'",
      reservedNames =
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
    }

-- | Token parser built from the language definition.
lexer :: TokenParser ()
lexer = makeTokenParser languageDef

-- | Parse something enclosed in parentheses.
parens' :: Parser a -> Parser a
parens' = parens lexer

-- | Parse an identifier.
identifier' :: Parser String
identifier' = identifier lexer

-- | Parse a reserved keyword.
reserved' :: String -> Parser ()
reserved' = reserved lexer

-- | Parse a reserved operator.
reservedOp' :: String -> Parser ()
reservedOp' = reservedOp lexer

-- | Parse a string literal.
stringLiteral' :: Parser String
stringLiteral' = stringLiteral lexer

-- | Parse a number (integer or float).
naturalOrFloat' :: Parser (Either Integer Double)
naturalOrFloat' = naturalOrFloat lexer

-- | Parse whitespace and comments.
whiteSpace' :: Parser ()
whiteSpace' = whiteSpace lexer

-- | Parse a specific symbol.
symbol' :: String -> Parser String
symbol' = symbol lexer

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
  ts <- many1 parseType
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
    <|> try (string "()" >> return PUnit)
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
  body <- many1 parseExpr
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
  body <- many1 parseExpr
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
  es <- many1 parseExpr
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
  ctors <- many1 parseCtor
  return $ EDefType name params ctors

-- | Parse a pattern matching expression: @(match expr (pattern body...)...)@.
parseMatch :: Parser Expr
parseMatch = do
  reserved' "match"
  expr <- parseExpr
  clauses <-
    many1
      ( parens' $ do
          pat <- parsePattern
          body <- many1 parseExpr
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
  look <- optionMaybe (lookAhead (many1 (letter <|> oneOf "+-*/<>=!?_-'")))
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
    <|> try (parens' (do _ <- char '\''; EQuote <$> parseExpr)) -- '( ...) rare
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
parse :: String -> Either ParseError Programs
parse = TP.parse parseProgram ""
