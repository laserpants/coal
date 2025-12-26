{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Language.Module.Path (
  Path (..),
  principalPath,
  parsePath,
  toFilePath,
) where

import Data.Binary (Binary)
import Data.Char (isAlphaNum, isUpper)
import Data.Data (Data, Typeable)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name)
import GHC.Generics (Generic)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance Binary Path

{-# INLINE principalPath #-}
principalPath :: Path -> Name
principalPath Path{..} = Text.intercalate "." pathComponents

-- | Parse and validate a module path
parsePath :: Text -> Maybe Path
parsePath input
  | Text.null input = Nothing
  | null comps = Nothing
  | otherwise = Path <$> traverse validateComponent comps
 where
  comps = Text.splitOn "." input

validateComponent :: Text -> Maybe Name
validateComponent t = do
  (c, rest) <- Text.uncons t
  if isUpper c && Text.all validChar rest
    then Just t
    else Nothing
 where
  validChar c = isAlphaNum c || c == '_'

toFilePath :: Path -> FilePath
toFilePath Path{..} = Text.unpack (Text.intercalate "/" pathComponents) <> ".coal"
