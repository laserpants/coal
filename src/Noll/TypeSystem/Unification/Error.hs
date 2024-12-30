{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Unification.Error (UnificationError (..)) where

data UnificationError
  = CannotUnify
  | InfiniteType
  | CannotUnifyKinds
  deriving (Show, Eq, Ord, Read)
