{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.Parser.Module (module_) where

import Coal.LegacyKernel.Language.Expr (Expr)
import Coal.LegacyKernel.Language.Module (Module (Module))
import Coal.LegacyKernel.Language.Object (Object (..))
import Coal.LegacyKernel.Language.Type (Type (..))
import Coal.LegacyKernel.Parser (Parser, lexeme, many, some)
import Coal.LegacyKernel.Parser.Expr (expr, label)
import Coal.LegacyKernel.Parser.Identifier (identifier)
import Coal.LegacyKernel.Parser.Symbol (angleBrackets, braces, commaSep1, parens, symbol)
import Coal.LegacyKernel.Parser.Type (type_)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Extras (Name)
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
