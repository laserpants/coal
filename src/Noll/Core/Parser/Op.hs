{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Parser.Op (op) where

import Noll.Core.Language.Op (Op (..))
import Noll.Core.Parser (Parser, lexeme, ($>), (<|>))
import Noll.Core.Parser.Symbol (brackets, pair, symbol)

op2symbol :: Parser (a -> a -> Op a)
op2symbol =
  (symbol "==" >> lexeme "int32" $> OEqInt32)
    <|> (symbol "<" >> lexeme "int32" $> OLtInt32)
    <|> (symbol ">" >> lexeme "int32" $> OGtInt32)
    <|> (symbol "+" >> lexeme "int32" $> OAddInt32)
    <|> (symbol "-" >> lexeme "int32" $> OSubInt32)
    <|> (symbol "*" >> lexeme "int32" $> OMulInt32)
    <|> (symbol "||" $> OOr)
    <|> (symbol "&&" $> OAnd)

op :: Parser a -> Parser (Op a)
op p = binop
 where
  binop = uncurry <$> brackets op2symbol <*> pair p p
