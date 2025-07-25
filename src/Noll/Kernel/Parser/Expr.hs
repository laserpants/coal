{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.Parser.Expr (expr, label) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr (makeExprParser)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Common.Label (Label (..))
import Noll.Kernel.Language.Expr (Binding (..), Clause (..), Expr, Focus (..))
import Noll.Kernel.Language.Type (Type (..))
import Noll.Kernel.Parser (Parser, lexeme, some, try, ($>), (<|>))
import Noll.Kernel.Parser.Identifier (constructor, name)
import Noll.Kernel.Parser.Op (op)
import Noll.Kernel.Parser.Prim (prim)
import Noll.Kernel.Parser.Symbol
import Noll.Kernel.Parser.Type (type_)
import Extra (Name)
import Text.Megaparsec (takeWhileP)
import Text.Megaparsec.Char (char)

import qualified Noll.Kernel.Language.Expr.Syntax as Core

label :: Parser t -> Parser (Label t)
label p = do
  l <- backtickString <|> name <|> constructor
  t <- colon *> p
  pure (Label t l)

backtickString :: Parser Name
backtickString =
  lexeme $ do
    void (char '`')
    s <- takeWhileP (Just "Non-backtick character") (/= '`')
    void (char '`')
    pure s

binding :: Parser e -> Parser (Binding Type e)
binding p = Binding <$> (label type_ <* equalSign) <*> p

let_ :: Parser (Expr Type) -> Parser (Expr Type)
let_ p = do
  void (lexeme "let")
  semicolonSep1 (binding p)
    >>= \case
      b : bs -> do
        void (lexeme "in")
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
  void (lexeme "fn")
  args <- parens (commaSep1 (label type_))
  void (symbol "=>")
  case args of
    a : as ->
      Core.lam (a :| as) <$> p
    _ ->
      fail "Empty list"

app :: Parser (Expr Type) -> Parser (Expr Type)
app p = do
  void (symbol "@")
  t <- angleBrackets type_
  parens (commaSep1 p)
    >>= \case
      e1 : e2 : es ->
        pure (Core.app t e1 (e2 :| es))
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
