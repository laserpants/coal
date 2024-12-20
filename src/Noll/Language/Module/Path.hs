{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Path (Path (..)) where

import Noll.Language (Name)

newtype Path = Path [Name]
  deriving (Show, Eq, Ord, Read)
