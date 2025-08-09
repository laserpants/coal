{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Utils (fieldListWithKey, fieldList) where

import Coal.Parser
import Coal.Parser.Identifier
import Coal.Parser.Symbol
import Data.Text (Text)
import Extra (Name)

fieldListWithKey :: Parser k -> Parser f -> Text -> Parser [(k, f)]
fieldListWithKey parseKey parseField sep = commaSep1 field
 where
  field = (,) <$> parseKey <*> (symbol_ sep *> parseField)

fieldList :: Parser f -> Text -> Parser [(Name, f)]
fieldList = fieldListWithKey name
