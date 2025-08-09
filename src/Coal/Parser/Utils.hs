{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Utils (fieldListWithKey, fieldList) where

import Coal.Parser
import Coal.Parser.Symbol
import Coal.Parser.Identifier
import Extra (Name)
import Data.Text (Text)

fieldListWithKey :: Parser k -> Parser f -> Text -> Parser [(k, f)]
fieldListWithKey parseKey parseField sep = commaSep1 field
 where
  field = (,) <$> parseKey <*> (symbol_ sep *> parseField)

fieldList :: Parser f -> Text -> Parser [(Name, f)]
fieldList = fieldListWithKey name
