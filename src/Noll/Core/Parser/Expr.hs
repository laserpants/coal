{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Parser.Expr (expr) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Core.Language.Expr (Binding (..), Clause (..), Expr (..), Focus (..))
import Noll.Core.Language.Type (Type (..))
import Noll.Core.Language.Type.Row (extend)
import Noll.Core.Parser (Parser, lexeme, some, try, ($>), (<|>))
import Noll.Core.Parser.Identifier (constructor, name)
import Noll.Core.Parser.Op (op)
import Noll.Core.Parser.Prim (prim)
import Noll.Core.Parser.Symbol (angleBrackets, braces, colon, commaSep, commaSep1, commaSepN, equalSign, pair, parens, pipe, semicolonSep1, symbol)
import Noll.Core.Parser.Type (type_)
import Noll.Label (Label (..))
import Noll.Utils (optionalOr)

import qualified Noll.Core.Language.Expr.Syntax as Core

label :: Parser t -> Parser (Label t)
label p = flip Label <$> (name <|> constructor) <* colon <*> p

binding :: Parser e -> Parser (Binding Type e)
binding p = Binding <$> (label type_ <* equalSign) <*> p

let_ :: Parser (Expr Type) -> Parser (Expr Type)
let_ p = do
  lexeme "let"
  semicolonSep1 (binding p)
    >>= \case
      b : bs -> do
        lexeme "in"
        Core.let_ (b :| bs) <$> p
      _ ->
        fail "Empty list"

nil :: Parser (Expr Type)
nil = symbol "{}" $> Core.nil

if_ :: Parser (Expr Type) -> Parser (Expr Type)
if_ p =
  Core.if_
    <$> (lexeme "if" *> parens p)
    <*> (lexeme "then" *> p)
    <*> (lexeme "else" *> p)

{-# INLINE var #-}
var :: Parser (Expr Type)
var = Core.var <$> label type_

lam :: Parser (Expr Type) -> Parser (Expr Type)
lam p = do
  lexeme "fn"
  args <- parens (commaSep1 (label type_))
  symbol "=>"
  case args of
    a : as ->
      Core.lam (a :| as) <$> p
    _ ->
      fail "Empty list"

app :: Parser (Expr Type) -> Parser (Expr Type)
app p = do
  symbol "@"
  t <- angleBrackets type_
  parens (commaSep1 p)
    >>= \case
      e1 : e2 : es ->
        pure (Core.app t e1 (e2 :| es))
      _ ->
        fail "Too few expressions"

clause :: Parser (Expr Type) -> Parser (Clause Type (Expr Type))
clause p = do
  symbol "|"
  lls <- parens (commaSep1 (label type_))
  symbol "=>"
  case lls of
    l : ls ->
      Clause (l :| ls) <$> p
    _ ->
      fail "Empty list"

match :: Parser (Expr Type) -> Parser (Expr Type)
match p = do
  lexeme "match"
  t <- angleBrackets type_
  e <- parens p
  braces (some (clause p))
    >>= \case
      c : cs ->
        pure (Core.match t e (c :| cs))
      _ ->
        fail "Empty list"

focus :: Parser (Focus Type)
focus =
  Focus
    <$> name
    <*> (equalSign *> label type_)
    <*> (pipe *> label type_)

select :: Parser (Expr Type) -> Parser (Expr Type)
select p =
  Core.sel
    <$> (lexeme "select" *> braces focus)
    <*> (equalSign *> p)
    <*> (lexeme "in" *> p)

record :: Parser (Expr Type) -> Parser (Expr Type)
record p = inner braces
 where
  inner f =
    nil
      <|> try var
      <|> f ext

  ext =
    Core.ext
      <$> (name <* equalSign)
      <*> (p <* pipe)
      <*> inner id

  nil =
    lexeme "{}" $> Core.nil

call :: Parser (Expr Type) -> Parser (Expr Type)
call p = do
  void (symbol "#")
  uncurry Core.call
    <$> pair (label type_) (commaSep1 p)
    <*> parens p

atom :: Parser (Expr Type) -> Parser (Expr Type)
atom p =
  (Core.lit <$> prim)
    <|> let_ p
    <|> if_ p
    <|> lam p
    <|> app p
    <|> match p
    <|> select p
    <|> (Core.op <$> op p)
    <|> record p
    <|> var
    <|> call p

expr :: Parser (Expr Type)
expr = makeExprParser (try (parens expr) <|> atom expr) [[]]
