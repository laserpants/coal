{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRValue (IRValue (..), irPrimValue) where

import Data.Int (Int32, Int64)
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Core.Language (Prim (..))
import Noll.Utils (Name)

data IRValue
  = Local IRType Name
  | Global IRType Name
  | I1 Bool
  | I32 Int32
  | I64 Int64
  | Float Float
  | Double Double
  | Null
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
    PChar _ ->
      error "Implementation error"
    PString _ ->
      error "Implementation error"
