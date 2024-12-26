{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeUnification.Error (UnificationError (..)) where

data UnificationError
  = CannotUnify
  | InfiniteType
  | CannotUnifyKinds
  deriving (Show, Eq, Ord, Read)
