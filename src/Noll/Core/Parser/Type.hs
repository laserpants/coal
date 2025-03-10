{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Parser.Type (type_) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Noll.Core.Language.Type (Type (..))
import Noll.Core.Language.Type.Row (extend)
import Noll.Core.Parser (Parser, lexeme, try, ($>), (<|>))
import Noll.Core.Parser.Identifier (constructor, name)
import Noll.Core.Parser.Symbol (braces, colon, commaSep, commaSepN, parens, pipe, symbol)
import Noll.Utils (optionalOr)

import qualified Noll.Core.Language.Type.Syntax as Type
import qualified Text.Megaparsec.Char.Lexer as Lexer

{-# INLINE opaque #-}
opaque :: Parser Type
opaque = symbol "*" $> Type.opaque

{-# INLINE bool #-}
bool :: Parser Type
bool = lexeme "bool" $> Type.bool

{-# INLINE char #-}
char :: Parser Type
char = lexeme "char" $> Type.char

{-# INLINE double #-}
double :: Parser Type
double = lexeme "double" $> Type.double

{-# INLINE float #-}
float :: Parser Type
float = lexeme "float" $> Type.float

{-# INLINE int32 #-}
int32 :: Parser Type
int32 = lexeme "int32" $> Type.int32

{-# INLINE int64 #-}
int64 :: Parser Type
int64 = lexeme "int64" $> Type.int64

{-# INLINE string #-}
string :: Parser Type
string = lexeme "string" $> Type.string

{-# INLINE unit #-}
unit :: Parser Type
unit = lexeme "unit" $> Type.unit

list :: Parser Type -> Parser Type
list p = lexeme "list" >> Type.list <$> parens p

tuple :: Parser Type -> Parser Type
tuple p = do
  lexeme "tuple"
  n <- lexeme Lexer.decimal
  if n >= 2
    then parens (Type.tuple n <$> commaSepN n p)
    else fail "Invalid tuple size"

row :: Parser Type -> Parser Type
row p = inner braces
 where
  inner :: (Parser Type -> Parser Type) -> Parser Type
  inner f = nil <|> opaque <|> f ext

  ext :: Parser Type
  ext =
    extend
      <$> (name <* colon)
      <*> (p <* pipe)
      <*> inner id

  nil :: Parser Type
  nil = lexeme "{}" $> RNil

record :: Parser Type -> Parser Type
record p = lexeme "record" >> Type.record <$> parens (row p)

constr :: Parser Type -> Parser Type
constr p = do
  name <- constructor
  TCon name <$> optionalOr [] (parens (commaSep p))

atom :: Parser Type -> Parser Type
atom p =
  opaque
    <|> bool
    <|> char
    <|> double
    <|> float
    <|> int32
    <|> int64
    <|> string
    <|> unit
    <|> list p
    <|> tuple p
    <|> row p
    <|> record p
    <|> constr p

operators :: [[Operator Parser Type]]
operators = [[InfixR (Type.arrow <$ symbol "/")]]

type_ :: Parser Type
type_ = makeExprParser (try (parens type_) <|> atom type_) operators
