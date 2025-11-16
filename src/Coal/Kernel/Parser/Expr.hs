{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Parser.Expr (expr, label) where

import Coal.Common.Label (Label (..))
import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr, Focus (..))
import qualified Coal.Kernel.Language.Expr.Syntax as Syntax
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Parser (Parser, backtickString, lexeme, some, try, ($>), (<|>))
import Coal.Kernel.Parser.Identifier (constructor, name)
import Coal.Kernel.Parser.Op (op)
import Coal.Kernel.Parser.Prim (prim)
import Coal.Kernel.Parser.Symbol
import Coal.Kernel.Parser.Type (type_)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (makeExprParser)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name)

label :: Parser t -> Parser (Label t)
label p = do
  l <- backtickString <|> name <|> constructor
  t <- colon *> p
  pure (Label t l)

binding :: Parser e -> Parser (Binding Type e)
binding p = Binding <$> (label type_ <* equalSign) <*> p

let_ :: Parser (Expr Type) -> Parser (Expr Type)
let_ p = do
  void (lexeme "let")
  semicolonSep1 (binding p)
    >>= \case
      b : bs -> do
        void (lexeme "in")
        Syntax.let_ (b :| bs) <$> p
      _ ->
        fail "Empty list"

nil :: Parser (Expr Type)
nil = symbol "{}" $> Syntax.nil

if_ :: Parser (Expr Type) -> Parser (Expr Type)
if_ p =
  Syntax.if_
    <$> (lexeme "if" *> parens p)
    <*> (lexeme "then" *> p)
    <*> (lexeme "else" *> p)

{-# INLINE var #-}
var :: Parser (Expr Type)
var = Syntax.var <$> label type_

lam :: Parser (Expr Type) -> Parser (Expr Type)
lam p = do
  void (lexeme "fn")
  args <- parens (commaSep1 (label type_))
  void (symbol "=>")
  case args of
    a : as ->
      Syntax.lam (a :| as) <$> p
    _ ->
      fail "Empty list"

app :: Parser (Expr Type) -> Parser (Expr Type)
app p = do
  void (symbol "@")
  t <- angleBrackets type_
  parens (commaSep1 p)
    >>= \case
      e1 : e2 : es ->
        pure (Syntax.app t e1 (e2 :| es))
      _ ->
        fail "Too few expressions"

clause :: Parser (Expr Type) -> Parser (Clause Type (Expr Type))
clause p = do
  void (symbol "|")
  lls <- parens (commaSep1 (label type_))
  void (symbol "=>")
  case lls of
    l : ls ->
      Clause (l :| ls) <$> p
    _ ->
      fail "Empty list"

match :: Parser (Expr Type) -> Parser (Expr Type)
match p = do
  void (lexeme "match")
  t <- angleBrackets type_
  e <- parens p
  braces (some (clause p))
    >>= \case
      c : cs ->
        pure (Syntax.match t e (c :| cs))
      _ ->
        fail "Empty list"

focus :: Parser (Focus Type)
focus =
  Focus
    <$> field
    <*> (equalSign *> label type_)
    <*> (pipe *> label type_)

select :: Parser (Expr Type) -> Parser (Expr Type)
select p =
  Syntax.sel
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
    Syntax.ext
      <$> (field <* equalSign)
      <*> (p <* pipe)
      <*> inner id

field :: Parser Name
field = backtickString <|> name

call :: Parser (Expr Type) -> Parser (Expr Type)
call p = do
  void (symbol "#")
  uncurry Syntax.call
    <$> pair (label type_) (commaSep1 p)
    <*> parens p

atom :: Parser (Expr Type) -> Parser (Expr Type)
atom p =
  (Syntax.lit <$> prim)
    <|> let_ p
    <|> if_ p
    <|> lam p
    <|> app p
    <|> match p
    <|> select p
    <|> (Syntax.op <$> op p)
    <|> record p
    <|> var
    <|> call p

expr :: Parser (Expr Type)
expr = makeExprParser (try (parens expr) <|> atom expr) [[]]
