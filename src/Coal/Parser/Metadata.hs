module Coal.Parser.Metadata (withMetadata) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Parser
import Text.Megaparsec (getSourcePos)

withMetadata :: Parser (Metadata -> p) -> Parser p
withMetadata p = do
  start <- getSourcePos
  f <- p
  end <- getSourcePos
  pure $ f (Metadata start end)
