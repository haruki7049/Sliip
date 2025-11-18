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

import Text.Parsec (ParseError, (<|>),
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

data Expr
  = ENumber Integer
  | EFloat Double
  | EString String
  | EBool Bool
  | ESymbol String
  | EList [Expr]
  | EDefine String Expr
  | ELambda [Param] [Expr]
  | EIf Expr Expr Expr
  | ELet [(String, Expr)] [Expr]
  | ELetStar [(String, Expr)] [Expr]
  | ELetRec [(String, Expr)] [Expr]
  | EBegin [Expr]
  | EQuote Expr
  | EAscription Expr TypeExpr
  | EDefType String [String] [Ctor]
  | EMatch Expr [(Pattern, [Expr])]
  | EApp Expr [Expr]
  deriving (Show)

data Param = Param String (Maybe TypeExpr)
  deriving (Show)

data TypeExpr
  = TName String
  | TApp String [TypeExpr]
  | TArrow [TypeExpr] -- (-> a b c)
  deriving (Show)

data Ctor = Ctor String [TypeExpr]
  deriving (Show)

data Pattern
  = PVar String
  | PWildcard
  | PUnit
  | PCtor String [Pattern]
  deriving (Show)

type Programs = [Expr]

-- Lexer ---------------------------------------------------------------------

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

lexer :: TokenParser ()
lexer = makeTokenParser languageDef

parens' :: Parser a -> Parser a
parens' = parens lexer

identifier' :: Parser String
identifier' = identifier lexer

reserved' :: String -> Parser ()
reserved' = reserved lexer

reservedOp' :: String -> Parser ()
reservedOp' = reservedOp lexer

stringLiteral' :: Parser String
stringLiteral' = stringLiteral lexer

naturalOrFloat' :: Parser (Either Integer Double)
naturalOrFloat' = naturalOrFloat lexer

whiteSpace' :: Parser ()
whiteSpace' = whiteSpace lexer

symbol' :: String -> Parser String
symbol' = symbol lexer

-- Basic parsers --------------------------------------------------------------

parseNumber :: Parser Expr
parseNumber = do
  nf <- naturalOrFloat'
  return $ case nf of
    Left i -> ENumber i
    Right d -> EFloat d

parseString :: Parser Expr
parseString = EString <$> stringLiteral'

parseBool :: Parser Expr
parseBool =
  (reserved' "true" >> return (EBool True))
    <|> (reserved' "false" >> return (EBool False))

parseSymbol :: Parser Expr
parseSymbol = ESymbol <$> identifier'

-- Type parser ---------------------------------------------------------------

parseType :: Parser TypeExpr
parseType = try parseArrow <|> parseTypeAtom

parseTypeAtom :: Parser TypeExpr
parseTypeAtom =
  (parens' $ do
     name <- identifier'
     args <- many parseType
     return $ TApp name args)
    <|> (TName <$> identifier')

parseArrow :: Parser TypeExpr
parseArrow = parens' $ do
  _ <- symbol' "->"
  ts <- many1 parseType
  return $ TArrow ts

-- Pattern parser ------------------------------------------------------------

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

parseCtor :: Parser Ctor
parseCtor = parens' $ do
  name <- identifier'
  tys <- many parseType
  return $ Ctor name tys

-- Expressions ---------------------------------------------------------------

parseAscription :: Parser Expr
parseAscription = do
  reserved' "as"
  e <- parseExpr
  EAscription e <$> parseType

parseDefine :: Parser Expr
parseDefine = do
  reserved' "define"
  name <- identifier'
  EDefine name <$> parseExpr

parseLambda :: Parser Expr
parseLambda = do
  reserved' "lambda"
  params <- parens' (many parseParam)
  body <- many1 parseExpr
  return $ ELambda params body

parseParam :: Parser Param
parseParam =
  try
    ( parens' $ do
        n <- identifier'
        Param n . Just <$> parseType
    )
    <|> (Param <$> identifier' <*> pure Nothing)

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

parseIf :: Parser Expr
parseIf = do
  reserved' "if"
  c <- parseExpr
  t <- parseExpr
  EIf c t <$> parseExpr

parseBegin :: Parser Expr
parseBegin = do
  reserved' "begin"
  es <- many1 parseExpr
  return $ EBegin es

parseQuote :: Parser Expr
parseQuote = do
  reserved' "quote"
  EQuote <$> parseExpr

parseDefType :: Parser Expr
parseDefType = do
  reserved' "def-type"
  name <- identifier'
  params <- parens' (many identifier')
  ctors <- many1 parseCtor
  return $ EDefType name params ctors

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

parseAtom :: Parser Expr
parseAtom =
  try parseNumber
    <|> try parseString
    <|> try parseBool
    <|> try (parens' (do _ <- char '\''; EQuote <$> parseExpr)) -- '( ...) rare
    <|> parseSymbol

parseExpr :: Parser Expr
parseExpr =
  whiteSpace'
    *> ( try parseAtom
           <|> try parseApplicationOrSpecial
       )

-- Program -------------------------------------------------------------------

parseProgram :: Parser Programs
parseProgram = whiteSpace' *> many parseExpr <* eof

-- Public API ----------------------------------------------------------------

parse :: String -> Either ParseError Programs
parse = TP.parse parseProgram ""
