{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Parser.Op (op) where

import Noll.Core.Language.Op (Op (..))
import Noll.Core.Parser (Parser, lexeme, try, ($>), (<|>))
import Noll.Core.Parser.Symbol (brackets, pair, parens, symbol)

op2symbol :: Parser (a -> a -> Op a)
op2symbol =
  (symbol "==" >> lexeme "int32" $> OEqInt32)
    <|> (symbol "!=" >> lexeme "int32" $> ONeInt32)
    <|> (symbol "<" >> lexeme "int32" $> OLtInt32)
    <|> (symbol ">" >> lexeme "int32" $> OGtInt32)
    <|> (symbol "+" >> lexeme "int32" $> OAddInt32)
    <|> (symbol "-" >> lexeme "int32" $> OSubInt32)
    <|> (symbol "*" >> lexeme "int32" $> OMulInt32)
    --
    <|> (symbol "!=" >> lexeme "int64" $> ONeInt64)
    <|> (symbol "==" >> lexeme "int64" $> OEqInt64)
    <|> (symbol "<" >> lexeme "int64" $> OLtInt64)
    <|> (symbol ">" >> lexeme "int64" $> OGtInt64)
    <|> (symbol "+" >> lexeme "int64" $> OAddInt64)
    <|> (symbol "-" >> lexeme "int64" $> OSubInt64)
    <|> (symbol "*" >> lexeme "int64" $> OMulInt64)
    --
    <|> (symbol "||" $> OOr)
    <|> (symbol "&&" $> OAnd)

op1symbol :: Parser (a -> Op a)
op1symbol = symbol "!" $> ONot

op :: Parser a -> Parser (Op a)
op p =
  try (brackets op1symbol <*> parens p)
    <|> (uncurry <$> brackets op2symbol <*> pair p p)
