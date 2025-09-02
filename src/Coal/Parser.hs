{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser (
  Parser,
  ParserError,
  cons,
  spaces,
  word,
  lexeme,
  lexeme_,
  nonEmpty,
  nonEmptyOr,
) where

import Control.Monad (void, when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Void (Void)
import Extra (Name)
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

{-# INLINE lexeme_ #-}
lexeme_ :: Parser a -> Parser ()
lexeme_ = void . lexeme

reserved :: [Name]
reserved =
  [ "let"
  , "in"
  , "fun"
  , "fn"
  , "fold"
  , "unfold"
  , "as"
  , "if"
  , "then"
  , "else"
  , "match"
  , "with"
  , "when"
  , "where"
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
      w <- p
      when (w `elem` reserved) $
        fail ("Reserved keyword " <> Text.unpack w)
      pure w

{-# INLINE cons #-}
cons :: Parser a -> Parser [a] -> Parser [a]
cons p ps = (:) <$> p <*> ps

nonEmpty :: Parser [a] -> Parser (NonEmpty a)
nonEmpty = nonEmptyOr (fail "Empty list")

nonEmptyOr :: Parser (NonEmpty a) -> Parser [a] -> Parser (NonEmpty a)
nonEmptyOr ls p = do
  ps <- p
  case ps of
    q : qs ->
      pure (q :| qs)
    [] ->
      ls
