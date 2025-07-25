{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.Parser.Prim (prim) where

import Control.Monad (void)
import Data.Char (ord)
import Noll.Kernel.Language.Prim (Prim (..))
import Noll.Kernel.Parser (Parser, lexeme, spaces, try, ($>), (<|>))
import Noll.Kernel.Parser.Symbol (symbol)
import Text.Megaparsec (manyTill)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

bool :: Parser Prim
bool = lexeme "true" $> PBool True <|> lexeme "false" $> PBool False

unit :: Parser Prim
unit = symbol "()" $> PUnit

squote :: Parser Char
squote = Megaparsec.char '\''

dquote :: Parser Char
dquote = Megaparsec.char '"'

char :: Parser Prim
char = do
  lexeme $ do
    void squote
    c <- Lexer.charLiteral
    void squote
    pure (PChar (fromIntegral (ord c)))

string :: Parser Prim
string = do
  lexeme $ do
    void dquote
    chars <- manyTill Lexer.charLiteral dquote
    pure (PString (Text.encodeUtf8 (Text.pack chars)))

float :: Parser Prim
float = PFloat <$> lexeme (Lexer.float <* (Megaparsec.char 'f' <|> Megaparsec.char 'F'))

double :: Parser Prim
double = PDouble <$> lexeme (Lexer.float :: Parser Double)

int32 :: Parser Prim
int32 = PInt32 <$> Lexer.signed spaces (lexeme Lexer.decimal)

int64 :: Parser Prim
int64 = symbol "%" *> (PInt64 <$> Lexer.signed spaces (lexeme Lexer.decimal))

bignum :: Parser Prim
bignum = symbol "%%" *> (PBignum <$> Lexer.signed spaces (lexeme Lexer.decimal))

prim :: Parser Prim
prim =
  unit
    <|> bool
    <|> char
    <|> string
    <|> try bignum
    <|> int64
    <|> try float
    <|> try double
    <|> int32
