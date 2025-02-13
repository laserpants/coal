{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Op where

-- | Binary operators
data Op a
  = -- | Equality
    OEqInt32 a a
  | OEqInt64 a a
  -- TODO
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
