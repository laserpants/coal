{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.Parser.Module (module_) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Noll.Kernel.Language.Expr (Expr)
import Noll.Kernel.Language.Module (Module (Module))
import Noll.Kernel.Language.Object (Object (..))
import Noll.Kernel.Language.Type (Type (..))
import Noll.Kernel.Parser (Parser, lexeme, many, some)
import Noll.Kernel.Parser.Expr (expr, label)
import Noll.Kernel.Parser.Identifier (identifier)
import Noll.Kernel.Parser.Symbol (angleBrackets, braces, commaSep1, parens, symbol)
import Noll.Kernel.Parser.Type (type_)
import Extra (Name)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (upperChar)

import qualified Text.Megaparsec.Char.Lexer as Lexer

{-# INLINE name #-}
name :: Parser Name
name = identifier upperChar

module_ :: Parser (Module Type Name (Expr Type))
module_ = do
  void (lexeme "module")
  n <- name
  (imports, objects) <- braces $ do
    (,)
      <$> many (lexeme "import" *> name)
      <*> some object
  pure $ Module n imports objects

{-# INLINE object #-}
object :: Parser (Object Type (Expr Type))
object = data_ <|> try constant <|> function

data_ :: Parser (Object Type (Expr Type))
data_ = do
  void (lexeme "data")
  n <- name
  (a, t) <- angleBrackets $ do
    a <- lexeme Lexer.decimal <* symbol ","
    t <- type_
    pure (a, t)
  pure (OData n a t)

constant :: Parser (Object Type (Expr Type))
constant = OConstant <$> name <*> (symbol "=" *> expr)

function :: Parser (Object Type (Expr Type))
function =
  OFunction
    <$> name
    <*> parens (commaSep1 (label type_))
    <*> (symbol "=" *> expr)
