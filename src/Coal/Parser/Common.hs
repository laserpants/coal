{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Parser.Common

Common parser utilities shared across multiple parser modules.

Provides utilities for parsing constructor names that are used by both the
expression and pattern parsers.
-}
module Coal.Parser.Common (
  parseSimpleConstructor,
  parseQualifiedConstructor,
) where

import Coal.Common.Label (Label (..))
import Coal.Parser.Core (Parser)
import Coal.Parser.Identifier (constructor)
import Coal.Parser.Symbol (symbol)
import qualified Data.Text as Text
import Text.Megaparsec (notFollowedBy, sepBy1, try)
import Text.Megaparsec.Char (char)

-- | Parse a simple (unqualified) constructor name
parseSimpleConstructor :: Parser (Label ())
parseSimpleConstructor = Label () <$> constructor

{- | Parse a qualified constructor name (e.g. @Module.Submodule.Constructor@).

The dot is the /separator/ rather than part of each segment. With the dot
inside the repeated element (@some (identifier upperChar <* symbol ".")@), the
final segment — which has no trailing dot — consumes input and then fails, and
the resulting consuming failure makes the repetition itself fail. That form
rejected /every/ input, so qualified constructors never parsed. Keeping the dot
in separator position lets the repetition stop cleanly at the end of the chain.

@sepBy1@ also accepts a single segment, so an unqualified name is handled here
too. The @notFollowedBy@ guard mirrors
'Coal.Parser.Expression.parseQualifiedName' and keeps @Module.Constructor .| f@
parseable as a qualified constructor followed by the @.|@ operator.
-}
parseQualifiedConstructor :: Parser (Label ())
parseQualifiedConstructor = do
  ns <- constructor `sepBy1` try (symbol "." <* notFollowedBy (char '|'))
  pure (Label () (Text.intercalate "." ns))
