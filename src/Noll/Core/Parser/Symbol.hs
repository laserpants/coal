{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Parser.Symbol (
  symbol,
  parens,
  brackets,
  braces,
  angleBrackets,
  commaSep,
  commaSep1,
  commaSep2,
  commaSepN,
  semicolonSep1,
  pipe,
  colon,
  slash,
  equalSign,
) where

import Noll.Core.Parser (Parser, Text, between, cons, sepBy, sepBy1, spaces)

import qualified Text.Megaparsec.Char.Lexer as Lexer

{-# INLINE symbol #-}
symbol :: Text -> Parser Text
symbol = Lexer.symbol spaces

parens :: Parser a -> Parser a
parens = symbol "(" `between` symbol ")"

brackets :: Parser a -> Parser a
brackets = symbol "[" `between` symbol "]"

braces :: Parser a -> Parser a
braces = symbol "{" `between` symbol "}"

angleBrackets :: Parser a -> Parser a
angleBrackets = symbol "<" `between` symbol ">"

commaSep :: Parser a -> Parser [a]
commaSep parser = parser `sepBy` symbol ","

commaSep1 :: Parser a -> Parser [a]
commaSep1 parser = parser `sepBy1` symbol ","

commaSep2 :: Parser a -> Parser [a]
commaSep2 parser = cons (parser <* symbol ",") (commaSep1 parser)

commaSepN :: Int -> Parser a -> Parser [a]
commaSepN n parser = do
  as <- commaSep parser
  if length as == n
    then pure as
    else fail "Wrong count"

semicolonSep1 :: Parser a -> Parser [a]
semicolonSep1 parser = parser `sepBy1` symbol ";"

{-# INLINE pipe #-}
pipe :: Parser Text
pipe = symbol "|"

{-# INLINE colon #-}
colon :: Parser Text
colon = symbol ":"

{-# INLINE slash #-}
slash :: Parser Text
slash = symbol "/"

{-# INLINE equalSign #-}
equalSign :: Parser Text
equalSign = symbol "="
