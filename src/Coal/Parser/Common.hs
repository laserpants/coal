{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Parser.Common

Common parser utilities shared across multiple parser modules.

Provides utilities for parsing qualified names and constructors that are
used by both expression and pattern parsers.
-}
module Coal.Parser.Common (
  parseSimpleConstructor,
  parseQualifiedConstructor,
  qualifiedName,
) where

import Coal.Common.Label (Label (..))
import Coal.Parser.Core (Parser)
import Coal.Parser.Identifier (constructor, identifier)
import Coal.Parser.Symbol (symbol)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (some)
import Text.Megaparsec.Char (upperChar)

-- | Parse a simple (unqualified) constructor name
parseSimpleConstructor :: Parser (Label ())
parseSimpleConstructor = Label () <$> constructor

-- | Parse a qualified constructor name (e.g., Module.Submodule.Constructor)
parseQualifiedConstructor :: Parser (Label ())
parseQualifiedConstructor = do
  ns <- some (identifier upperChar <* symbol ".")
  n <- constructor
  pure (Label () (Text.intercalate "." ns <> "." <> n))

{- | Parse a qualified name with a given initial character parser

Example: qualifiedName lowerChar parses qualified lowercase identifiers
like "Module.function"
-}
qualifiedName :: Parser Char -> Parser Text
qualifiedName initialParser = do
  ns <- some (identifier initialParser <* symbol ".")
  n <- identifier initialParser
  pure (Text.intercalate "." ns <> "." <> n)
