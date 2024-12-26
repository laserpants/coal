{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeUnification.Error (UnificationError (..)) where

data UnificationError
  = CannotUnify
  | InfiniteType
  deriving (Show, Eq, Ord, Read)
