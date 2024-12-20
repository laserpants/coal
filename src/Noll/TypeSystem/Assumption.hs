{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Assumption (Assumption (..)) where

data Assumption t = Assumption
  { assumptionName :: Name
  , assumptionType :: t
  }
  deriving (Show, Eq, Ord, Read)
