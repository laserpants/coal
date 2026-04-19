{- |
Module: Coal.Parser.Utils

Utilities for parsing field lists.

Provides combinators for parsing comma-separated field lists with keys,
commonly used in record types and patterns.
-}
module Coal.Parser.Utils (fieldListWithKey, fieldList) where

import Coal.Parser.Core (Parser)
import Coal.Parser.Identifier (name)
import Coal.Parser.Symbol (commaSep1, symbol_)
import Data.Text (Text)
import Extras (Name)

fieldListWithKey :: Parser k -> Parser f -> Text -> Parser [(k, f)]
fieldListWithKey parseKey parseField sep = commaSep1 field
 where
  field = (,) <$> parseKey <*> (symbol_ sep *> parseField)

{-# INLINE fieldList #-}
fieldList :: Parser f -> Text -> Parser [(Name, f)]
fieldList = fieldListWithKey name
