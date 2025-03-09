{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRConstruct (
  IRConstruct (..),
  IRLinkage (..),
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Noll.Core.LLVM.IRType (IRType)
import Noll.Core.LLVM.IRValue (IRValue)
import Noll.Label (Label (..))
import Noll.Utils (Name)

-- | <https://www.llvm.org/docs/LangRef.html#linkage-types>
data IRLinkage
  = -- | Internal linkage
    LInternal
  | -- Private linkage
    LPrivate
  deriving (Show, Eq, Ord)

-- | Top-level IR language construct
data IRConstruct a
  = -- | External symbol
    CDeclare Name IRType [IRType]
  | -- | IR type definition
    CType Name IRType
  | -- | Global symbol
    CGlobal Name IRType (Maybe IRLinkage) IRValue
  | -- | Top-level string constant
    CString Name ByteString
  | -- | Function definition
    CDefine Name IRType (Maybe IRLinkage) [Label IRType] a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
