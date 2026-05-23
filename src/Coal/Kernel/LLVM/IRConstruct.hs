{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.LLVM.IRConstruct (
  IRConstruct (..),
  IRLinkage (..),
) where

import Coal.Common.Label (Label (..))
import Coal.Kernel.LLVM.IRType (IRType)
import Coal.Kernel.LLVM.IRValue (IRValue)
import Data.ByteString (ByteString)
import Extras (Name)

-- | <https://www.llvm.org/docs/LangRef.html#linkage-types>
data IRLinkage
  = -- | Internal linkage
    LInternal
  | -- Private linkage
    LPrivate
  deriving (Show, Eq, Ord)

-- | Top-level IR language constructs
data IRConstruct a
  = -- | External symbol declaration (function)
    CDeclare Name IRType [IRType]
  | -- | External global variable declaration
    CExternGlobal Name IRType
  | -- | IR type definition
    CType Name IRType
  | -- | Global symbol
    CGlobal Name IRType (Maybe IRLinkage) IRValue
  | -- | Top-level string constant (Nothing = external linkage, Just l = explicit linkage)
    CString Name ByteString (Maybe IRLinkage)
  | -- | Function definition
    CDefine Name IRType (Maybe IRLinkage) [Label IRType] a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
