{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Path (Path (..)) where

import Noll.Utils (Name)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read)
