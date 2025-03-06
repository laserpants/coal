{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRConstruct (
  IRConstruct (..),
  IRLinkage (..),
) where

import Data.Text (Text)
import Noll.Core.LLVM.IRType (IRType)
import Noll.Core.LLVM.IRValue (IRValue)
import Noll.Label (Label (..))
import Noll.Utils (Name)

data IRLinkage
  = LInternal
  | LPrivate
  deriving (Show, Eq, Ord)

-- | Top-level IR language construct
data IRConstruct a
  = -- | External symbol
    CDeclare Name IRType [IRType]
  | -- | IR type definition
    CType Name IRType
  | -- | Global symbol
    CGlobal Name IRType IRValue
  | -- | Top-level string constant
    CString Name Text
  | -- | Function definition
    CDefine Name IRType (Maybe IRLinkage) [Label IRType] a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
