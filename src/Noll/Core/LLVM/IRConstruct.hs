{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRConstruct (
  IRConstruct (..),
  IRLinkage (..),
  sortIRConstructs,
) where

import Data.Function (on)
import Data.List (sortBy)
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
  = CDefine Name IRType (Maybe IRLinkage) [Label IRType] a
  | CDeclare Name IRType [IRType]
  | CType Name IRType
  | CString Name Text
  | CGlobal Name IRType IRValue
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

irConstructWeight :: IRConstruct a -> Int
irConstructWeight =
  \case
    CDeclare{} -> 1
    CType{} -> 2
    CGlobal{} -> 3
    CString{} -> 4
    CDefine{} -> 5

sortIRConstructs :: [IRConstruct a] -> [IRConstruct a]
sortIRConstructs = sortBy (compare `on` irConstructWeight)
