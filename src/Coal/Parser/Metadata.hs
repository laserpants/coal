module Coal.Parser.Metadata (withMetadata, withMetadataM) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Parser.Core (Parser)
import Text.Megaparsec (getSourcePos)

withMetadata :: Parser (Metadata -> p) -> Parser p
withMetadata = withMetadataM . fmap (pure .)

withMetadataM :: Parser (Metadata -> Parser p) -> Parser p
withMetadataM p = do
  start <- getSourcePos
  f <- p
  end <- getSourcePos
  f (Metadata start end)
