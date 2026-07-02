{-# LANGUAGE OverloadedStrings #-}

{- |
Symbol and delimiter parsers.

Provides lexeme-level parsers for punctuation, operators, and delimiters used
throughout the kernel language grammar.

All parsers consume trailing whitespace automatically.
-}
module Coal.Kernel.Parser.Symbol (
  colon,
  equals,
  semicolon,
  pipe,
  arrow,
  at,
  hash,
  slash,
  star,
  emptyBraces,
  angleBrackets,
  braces,
  semicolonSep1,
  commaSep1,
) where

import Control.Monad (void)

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

import Coal.Kernel.Parser (Parser, lexeme)

-- | Parse colon: :
colon :: Parser ()
colon = void $ lexeme (C.char ':')

-- | Parse equals: =
equals :: Parser ()
equals = void $ lexeme (C.char '=')

-- | Parse semicolon: ;
semicolon :: Parser ()
semicolon = void $ lexeme (C.char ';')

-- | Parse pipe: |
pipe :: Parser ()
pipe = void $ lexeme (C.char '|')

-- | Parse arrow: =>
arrow :: Parser ()
arrow = void $ lexeme (C.string "=>")

-- | Parse at symbol: @
at :: Parser ()
at = void $ lexeme (C.char '@')

-- | Parse hash symbol: #
hash :: Parser ()
hash = void $ lexeme (C.char '#')

-- | Parse slash: /
slash :: Parser ()
slash = void $ lexeme (C.char '/')

-- | Parse star: *
star :: Parser ()
star = void $ lexeme (C.char '*')

-- | Parse empty braces: {}
emptyBraces :: Parser ()
emptyBraces = void $ lexeme (C.string "{}")

-- | Parse angle brackets: < ... >
angleBrackets :: Parser a -> Parser a
angleBrackets p = do
  void $ lexeme (C.char '<')
  result <- p
  void $ lexeme (C.char '>')
  return result

-- | Parse braces: { ... }
braces :: Parser a -> Parser a
braces p = do
  void $ lexeme (C.char '{')
  result <- p
  void $ lexeme (C.char '}')
  return result

-- | Parse semicolon-separated list (at least one)
semicolonSep1 :: Parser a -> Parser [a]
semicolonSep1 p = P.sepBy1 p semicolon

-- | Parse comma-separated list (at least one)
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = P.sepBy1 p (lexeme (C.char ','))
