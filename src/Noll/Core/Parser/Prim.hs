{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Parser.Prim (prim) where

import Noll.Core.Language.Prim (Prim (..))
import Noll.Core.Parser (Parser, lexeme, spaces, try, ($>), (<|>))
import Noll.Core.Parser.Symbol (symbol)

import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

bool :: Parser Prim
bool = lexeme "true" $> PBool True <|> lexeme "false" $> PBool False

unit :: Parser Prim
unit = symbol "()" $> PUnit

char :: Parser Prim
char = undefined

string :: Parser Prim
string = undefined

float :: Parser Prim
float = PFloat <$> lexeme (Lexer.float <* (Megaparsec.char 'f' <|> Megaparsec.char 'F'))

double :: Parser Prim
double = PDouble <$> lexeme (Lexer.float :: Parser Double)

int32 :: Parser Prim
int32 = PInt32 <$> Lexer.signed spaces (lexeme Lexer.decimal)

int64 :: Parser Prim
int64 = symbol "!" *> (PInt64 <$> Lexer.signed spaces (lexeme Lexer.decimal))

prim :: Parser Prim
prim =
  unit
    <|> bool
    <|> int64
    <|> try float
    <|> try double
    <|> int32
