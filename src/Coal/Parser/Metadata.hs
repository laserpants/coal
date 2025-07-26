module Coal.Parser.Metadata (withMetadata) where

import Coal.Parser
import Coal.Ast.Metadata (Metadata (..))
import Text.Megaparsec (getSourcePos)

withMetadata :: Parser (Metadata -> p) -> Parser p
withMetadata p = do
  start <- getSourcePos
  f <- p
  end <- getSourcePos
  pure (f (Metadata start end))
