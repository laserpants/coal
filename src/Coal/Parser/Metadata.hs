{- |
Module: Coal.Parser.Metadata

Source location tracking helpers for parsers.

Provides combinators to attach source position metadata to parsed constructs.
-}
module Coal.Parser.Metadata (withMetadata, withMetadataM) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Parser.Core (Parser)
import Text.Megaparsec (getSourcePos)

withMetadata :: Parser (Metadata -> p) -> Parser p
withMetadata = withMetadataM . fmap (pure .)

withMetadataM :: Parser (Metadata -> Parser p) -> Parser p
withMetadataM parser = do
  start <- getSourcePos
  f <- parser
  end <- getSourcePos
  f (Metadata start end)
