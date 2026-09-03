{-# LANGUAGE OverloadedStrings #-}

module CLI.Command.EntryPoint (parseEntryPoint) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Parse an entry point string like "Main.main" into (moduleName, functionName)
parseEntryPoint :: Maybe Text -> Maybe (Text, Text)
parseEntryPoint = (parseDotSeparated =<<)
 where
  parseDotSeparated t =
    case Text.splitOn "." t of
      [mod_, func] -> Just (mod_, func)
      _ -> Nothing
