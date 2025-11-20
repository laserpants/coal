{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Language.Module.Path (Path (..), principalPath) where

import Data.Data (Data, Typeable)
import qualified Data.Text as Text
import Extras (Name)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read, Data, Typeable)

{-# INLINE principalPath #-}
principalPath :: Path -> Name
principalPath p = Text.intercalate "." path where Path path = p
