{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Path (Path (..)) where

import Noll.Language (Name)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read)
