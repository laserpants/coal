{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Noll.Library.Environment (Environment (..)) where

import Noll.Utils (Dictionary)

newtype Environment e = Environment {environmentDictionary :: Dictionary e}
  deriving (Show, Eq, Ord, Read, Semigroup, Monoid)
