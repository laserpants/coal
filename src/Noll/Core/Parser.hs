{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Parser (
  Parser,
  ParserError,
  cons,
  word,
  spaces,
  lexeme,
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
import Noll.Utils (Name)
import Text.Megaparsec (ParseErrorBundle, Parsec, between, optional, sepBy, sepBy1, some, try)
import Text.Megaparsec.Char (space1)

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

spaces :: Parser ()
spaces =
  Lexer.space
    space1
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
  ]

word :: Parser Text -> Parser Text
word parser =
  lexeme $
    try $ do
      name <- parser
      when (name `elem` reserved) $
        fail ("Reserved keyword " <> Text.unpack name)
      pure name

{-# INLINE cons #-}
cons :: Parser a -> Parser [a] -> Parser [a]
cons p ps = (:) <$> p <*> ps
