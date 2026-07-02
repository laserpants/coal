{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.Parser.Type (type_) where

import Coal.LegacyKernel.Language.Type (Type (..))
import Coal.LegacyKernel.Language.Type.Row (extend)
import qualified Coal.LegacyKernel.Language.Type.Syntax as Type
import Coal.LegacyKernel.Parser (Parser, backtickString, lexeme, try, ($>), (<|>))
import Coal.LegacyKernel.Parser.Identifier (constructor, name)
import Coal.LegacyKernel.Parser.Symbol (braces, colon, commaSep, commaSepN, parens, pipe, symbol)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Extras (Name, optionalOr)
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

{-# INLINE bignum #-}
bignum :: Parser Type
bignum = lexeme "bignum" $> Type.bignum

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
  void (lexeme "tuple")
  n <- lexeme Lexer.decimal
  if n >= 2
    then parens (Type.tuple <$> commaSepN n p)
    else fail "Invalid tuple size"

row :: Parser Type -> Parser Type
row p = inner braces
 where
  inner f = nil <|> opaque <|> f ext

  ext =
    extend
      <$> (field <* colon)
      <*> (p <* pipe)
      <*> inner id

  nil =
    lexeme "{}" $> RNil

field :: Parser Name
field = backtickString <|> name

record :: Parser Type -> Parser Type
record p = lexeme "record" >> Type.record <$> parens (row p)

constr :: Parser Type -> Parser Type
constr p = TCon <$> constructor <*> optionalOr [] (parens (commaSep p))

atom :: Parser Type -> Parser Type
atom p =
  opaque
    <|> bool
    <|> char
    <|> double
    <|> float
    <|> int32
    <|> int64
    <|> bignum
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
