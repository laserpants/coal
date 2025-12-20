{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Language.Module.Path (Path (..), principalPath) where

import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import qualified Data.Text as Text
import Extras (Name)
import GHC.Generics (Generic)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read, Data, Typeable, Generic)

instance Binary Path

{-# INLINE principalPath #-}
principalPath :: Path -> Name
principalPath Path{..} = Text.intercalate "." pathComponents
