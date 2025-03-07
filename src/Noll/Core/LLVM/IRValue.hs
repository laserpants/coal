{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRValue (IRValue (..), irPrimValue) where

import Data.Int (Int32, Int64)
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Core.Language (Prim (..))
import Noll.Utils (Name)

-- | LLVM IR values
data IRValue
  = -- | Local variable
    Local IRType Name
  | -- | Global variable
    Global IRType Name
  | -- | Single-bit integer
    I1 Bool
  | -- | 32-bit integer
    I32 Int32
  | -- | 64-bit integer
    I64 Int64
  | -- | Single-precision floating point number
    Float Float
  | -- | Double-precision floating point number
    Double Double
  | -- | Null value
    Null
  deriving (Show, Eq, Ord, Read)

instance IRTyped IRValue where
  irTypeOf =
    \case
      Local t _ ->
        t
      Global t _ ->
        t
      I1{} ->
        TInt1
      I32{} ->
        TInt32
      I64{} ->
        TInt64
      Float{} ->
        TFloat
      Double{} ->
        TDouble
      Null ->
        TPtr TInt8

irPrimValue :: Prim -> IRValue
irPrimValue =
  \case
    PBool b ->
      I1 b
    PInt32 n ->
      I32 n
    PInt64 n ->
      I64 n
    PFloat f ->
      Float f
    PDouble d ->
      Double d
    PUnit ->
      I1 True
    PChar c ->
      I32 c
    PString _ ->
      error "Implementation error"
