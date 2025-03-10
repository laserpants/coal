{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Parser.Prim (prim) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Core.Language.Expr (Binding (..), Expr (..))
import Noll.Core.Language.Prim (Prim (..))
import Noll.Core.Language.Type (Type (..))
import Noll.Core.Language.Type.Row (extend)
import Noll.Core.Parser (Parser, lexeme, spaces, try, ($>), (<|>))
import Noll.Core.Parser.Identifier (constructor, name)
import Noll.Core.Parser.Symbol (braces, colon, commaSep, commaSepN, equalSign, parens, pipe, semicolonSep1, symbol)
import Noll.Core.Parser.Type (type_)
import Noll.Label (Label (..))
import Noll.Utils (optionalOr)

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
