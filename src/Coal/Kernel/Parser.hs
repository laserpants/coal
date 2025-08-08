{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Parser (
  Parser,
  ParserError,
  cons,
  word,
  spaces,
  lexeme,
  spaced,
  backtickString,
  module Text.Megaparsec,
  module Data.Text,
  module Control.Applicative,
  module Data.Functor,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Functor (void, ($>), (<$), (<$>))
import Data.Text (Text)
import Data.Void (Void)
import Extra (Name)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, newline, spaceChar)

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

nonIndentedToken :: Parser Char
nonIndentedToken = do
  void newline
  alphaNumChar <|> char '_'

spaced :: Parser ()
spaced = void (oneOf (" \t" :: String) <* notFollowedBy nonIndentedToken <|> spaceChar)

spaces :: Parser ()
spaces =
  Lexer.space
    spaced
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "{-" "-}")

{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaces

reserved :: [Name]
reserved =
  [ "let"
  , "in"
  , "if"
  , "fn"
  , "then"
  , "else"
  , "match"
  , "true"
  , "false"
  , "unit"
  , "bool"
  , "int32"
  , "int64"
  , "float"
  , "double"
  , "char"
  , "string"
  , "select"
  , "data"
  , "import"
  , "module"
  ]

word :: Parser Text -> Parser Text
word p =
  lexeme $
    try $ do
      name <- p
      when (name `elem` reserved) $
        fail ("Reserved keyword " <> Text.unpack name)
      pure name

{-# INLINE cons #-}
cons :: Parser a -> Parser [a] -> Parser [a]
cons p ps = (:) <$> p <*> ps

backtickString :: Parser Name
backtickString =
  lexeme $ do
    void (char '`')
    s <- takeWhileP (Just "Non-backtick character") (/= '`')
    void (char '`')
    pure s
