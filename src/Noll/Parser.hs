{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser (
  Parser,
  ParserError,
  cons,
  spaces,
  word,
  lexeme,
) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Void (Void)
import Lang.Utils (Name)
import Text.Megaparsec
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
    (Lexer.skipBlockComment "/*" "*/")

{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaces

reserved :: [Name]
reserved =
  [ "let"
  , "in"
  , "fn"
  , "fold"
  , "unfold"
  , "as"
  , "if"
  , "then"
  , "else"
  , "match"
  , "uses"
  , "or"
  , "type"
  , "cotype"
  , "alias"
  , "trait"
  , "instance"
  , "module"
  , "import"
  , "true"
  , "false"
  , "not"
  , "unit"
  , "bool"
  , "int32"
  , "int64"
  , "bignum"
  , "float"
  , "double"
  , "char"
  , "string"
  , "nat"
  ]

word :: Parser Text -> Parser Text
word p =
  lexeme $
    try $ do
      txt <- p
      when (txt `elem` reserved) $
        fail ("Reserved keyword " <> Text.unpack txt)
      pure txt

{-# INLINE cons #-}
cons :: Parser a -> Parser [a] -> Parser [a]
cons p ps = (:) <$> p <*> ps
