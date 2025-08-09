{-# LANGUAGE OverloadedStrings #-}

module Coal.Parser.Utils (fieldList) where

import Coal.Parser
import Coal.Parser.Symbol
import Coal.Parser.Identifier
import Extra (Name)
import Data.Text (Text)

fieldList :: Parser f -> Text -> Parser [(Name, f)]
fieldList parseField sep = commaSep1 field
 where
  field = (,) <$> name <*> (symbol_ sep *> parseField)
