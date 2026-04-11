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

import Control.Monad (unless)
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

data PathError
  = EmptyComponent
  | InvalidStart Char
  | InvalidChar Char
  deriving (Show, Eq)

validateComponent :: Text -> Either PathError Name
validateComponent t = do
  (first, rest) <- maybe (Left EmptyComponent) Right (Text.uncons t)
  unless (isUpper first) $
    Left (InvalidStart first)
  case Text.find (not . validChar) rest of
    Just c ->
      Left (InvalidChar c)
    Nothing ->
      Right t
 where
  validChar c = isAlphaNum c || c == '_'

-- | Parse and validate a module path
parsePath :: Text -> Either PathError Path
parsePath input
  | Text.null input =
      Left EmptyComponent
  | otherwise = do
      let comps = Text.splitOn "." input
      names <- traverse validateComponent comps
      pure (Path names)

toFilePath :: Path -> FilePath
toFilePath Path{..} = Text.unpack (parts <.> "coal")
 where
  parts = Text.intercalate "/" pathComponents
