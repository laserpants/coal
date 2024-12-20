{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Assumption (Assumption (..)) where

import Noll.Language (Name)

data Assumption t = Assumption
  { assumptionName :: Name
  , assumptionType :: t
  }
  deriving (Show, Eq, Ord, Read)
