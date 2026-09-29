{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Parser.Core

Core parser infrastructure and basic combinators.

Provides the base Parser type, lexing utilities, and fundamental
combinators used throughout the parser implementation.
-}
module Coal.Parser.Core (
  Parser,
  ParserError,
  cons,
  spaces,
  word,
  lexeme,
  lexeme_,
  integer,
  nonEmpty,
  nonEmptyOr,
) where

import Control.Monad (void, when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Extras (Name)
import Text.Megaparsec (MonadParsec (try), ParseErrorBundle, Parsec, (<|>))
import Text.Megaparsec.Char (char, space1)
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

{- | Parse an optionally signed integer literal.

Hexadecimal literals use a @0x@ or @0X@ prefix (@0xbeef@, @0xBEEF@); all
other integer literals are decimal.
-}
integer :: Parser Integer
integer = Lexer.signed spaces (lexeme (hex <|> Lexer.decimal))
 where
  hex :: Parser Integer
  hex = try (char '0' *> (char 'x' <|> char 'X')) *> Lexer.hexadecimal

reserved :: [Name]
reserved =
  [ "let"
  , "in"
  , "fun"
  , "fn"
  , "fold"
  , "as"
  , "if"
  , "then"
  , "else"
  , "match"
  , "with"
  , "when"
  , "where"
  , "or"
  , "otherwise"
  , "type"
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
  , "do"
  ]

-- | Parse a word (identifier or keyword) and reject reserved keywords
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
