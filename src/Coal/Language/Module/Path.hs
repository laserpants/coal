{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Language.Module.Path (
  Path (..),
  principalPath,
  emptyPath,
  parsePath,
  toFilePath,
) where

import Data.Binary (Binary)
import Data.Char (isAlphaNum, isUpper)
import Data.Data (Data, Typeable)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name)
import Extras.Operators ((<.>))
import GHC.Generics (Generic)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance Binary Path

{-# INLINE emptyPath #-}
emptyPath :: Path
emptyPath = Path []

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

-- TODO: Combine this with code in Path.Resolve
validateComponent :: Text -> Maybe Name
validateComponent t = do
  (first, rest) <- Text.uncons t
  if isUpper first && Text.all validChar rest
    then Just t
    else Nothing
 where
  validChar c = isAlphaNum c || c == '_'

toFilePath :: Path -> FilePath
toFilePath Path{..} = Text.unpack (parts <.> "coal")
 where
  parts = Text.intercalate "/" pathComponents
