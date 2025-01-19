{-# LANGUAGE OverloadedStrings #-}

module Parser (parseSyntax) where

import Control.Applicative hiding (many, some)
import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (Parsec, between, many, choice, try)
import Text.Megaparsec.Char (alphaNumChar, lowerChar, space1)

import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Atom =
  TrueLiteral
  | FalseLiteral
  | NumLiteral Integer
  | Symbol String
  deriving (Eq, Show)

type Var = (String, QLType)

data QLType =
  Num | Truth | FnTy QLType QLType
  deriving (Eq, Show)

data SpecialForms =
  QuoteForm QLTerm |
  IfForm QLTerm QLTerm QLTerm |
  DefVar Var QLTerm |
  DefFn Var [Var] QLTerm
  deriving (Eq, Show)

data QLTerm =
  At Atom |
  FnCall String [QLTerm] |
  Special SpecialForms
  deriving (Eq, Show)

parseBasicType :: Parser QLType
parseBasicType = choice
  [
    Num <$ lexSym "Num",
    Truth <$ lexSym "Bool"
  ]

parseFnTy :: Parser QLType
parseFnTy = do
  first <- parseBasicType
  _ <- lexSym "->"
  rest <- parseBasicType <|> parseFnTy
  return (FnTy first rest)

parseType :: Parser QLType
parseType = parens parseFnTy <|> parseBasicType

parseVar :: Parser Var
parseVar = do
  name <- symbol'
  _ <- lexSym ":"
  typ <- parseType
  return (name, typ)


parseDefVar' = do
  _ <- lexSym "def"
  var <- parens parseVar
  Special . DefVar var <$> parseExpr

parseDefVar = parens parseDefVar'

parseDefFn :: Parser QLTerm
parseDefFn' = do
  _ <- lexSym "defn"
  var <- parens parseVar
  args <- brackets (many (parens parseVar))
  Special . DefFn var args <$> parseExpr

parseDefFn = parens parseDefFn'

  --- Lexing

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

blockComment :: Parser ()
blockComment = empty

sc :: Parser ()
sc =
  L.space
    space1
    lineComment
    blockComment


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexSym :: Text -> Parser Text
lexSym = L.symbol sc

parseTrue :: Parser Atom
parseTrue = TrueLiteral <$ lexSym "#t"

parseFalse :: Parser Atom
parseFalse = FalseLiteral <$ lexSym "#f"

parseBool :: Parser Atom
parseBool = parseTrue <|> parseFalse

integer :: Parser Integer
integer = lexeme (L.signed empty L.decimal)

parseInteger :: Parser Atom
parseInteger = NumLiteral <$> integer

symbol' :: Parser String
symbol' =  lexeme ((:) <$> lowerChar <*> many alphaNumChar)

symbol :: Parser Atom
symbol = Symbol <$> symbol'

atom :: Parser QLTerm
atom = At <$> (symbol <|> parseInteger <|> parseBool)

funcCall :: Parser QLTerm
funcCall = do
  name <- symbol'
  args <- many (atom <|> parens funcCall)
  return (FnCall name args)

parseIfForm :: Parser QLTerm
parseIfForm = do
  _ <- lexSym "if"
  cond <- parseExpr
  trueBranch <- parseExpr
  Special . IfForm cond trueBranch <$> parseExpr


parens :: Parser a -> Parser a
parens = between (lexSym "(") (lexSym ")")

brackets :: Parser a -> Parser a
brackets = between (lexSym "[") (lexSym "]")

parseExpr :: Parser QLTerm
parseExpr = atom <|> parens (try parseIfForm <|> funcCall)

parseSyntax :: Parser [QLTerm]
parseSyntax = many (try parseDefFn <|> parseDefVar)
