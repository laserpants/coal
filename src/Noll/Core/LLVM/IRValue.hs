{-# LANGUAGE LambdaCase #-}

module Noll.Core.LLVM.IRValue (IRValue (..)) where

import Data.Int (Int32, Int64)
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Utils (Name)

data IRValue
  = Local IRType Name
  | Constant IRType Name
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
      Constant t _ ->
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
