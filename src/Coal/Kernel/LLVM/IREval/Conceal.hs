{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Value concealment and revelation for uniform i8* representation

= Purpose

The runtime uses uniform @i8*@ pointers for all values. This module
provides conversion between native LLVM types and the uniform representation.

= Strategy

* __Integers__ (i1, i8, i32, i64): Embed directly in pointer via @inttoptr@
  No heap allocation needed - the value IS the pointer.

* __Floats/Doubles__: Box on heap with bit pattern preservation
  1. Cast float bits to integer (@castFloatToWord32@, @castDoubleToWord64@)
  2. Allocate heap memory
  3. Store integer bit pattern
  4. Return pointer to heap location
  This preserves exact float representation across concealment/revelation.

* __Pointers__: Pass through unchanged (already i8*)
-}
module Coal.Kernel.LLVM.IREval.Conceal (irConceal, irConcealArgs, irReveal) where

import Coal.Kernel.LLVM.IREval (IREval (..), IRTailContext (..))
import Coal.Kernel.LLVM.IREval.Comment (irComment)
import Coal.Kernel.LLVM.IREval.Malloc (irMalloc)
import Coal.Kernel.LLVM.IRInstruction (IRInstr)
import Coal.Kernel.LLVM.IRInstruction.Builders
import Coal.Kernel.LLVM.IRType (IRType (..), IRTyped (..))
import Coal.Kernel.LLVM.IRType.Syntax (i1, i32, i64, i8, i8Ptr, ptr)
import Coal.Kernel.LLVM.IRValue (IRValue (..))
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty, toList)
import Extras (forM)
import GHC.Float (castDoubleToWord64, castFloatToWord32)

irBox :: IRValue -> IRType -> IRInstr IRValue
irBox v t = do
  r1 <- irMalloc t
  store v r1
  bitcast r1 i8Ptr

irConcealFloat :: IRValue -> IRType -> IRInstr IRValue
irConcealFloat v t = do
  r1 <- irMalloc t
  tmp <- bitcast v t
  store tmp r1
  bitcast r1 i8Ptr

irConceal :: IRValue -> IRInstr IRValue
irConceal v =
  case irTypeOf v of
    TInt1 ->
      inttoptr v i8Ptr
    TInt8 ->
      inttoptr v i8Ptr
    TInt32 ->
      inttoptr v i8Ptr
    TInt64 ->
      inttoptr v i8Ptr
    TFloat -> do
      case v of
        -- Literal float: convert bit pattern to i32, then box
        Float f ->
          irConcealFloat (I32 (fromIntegral (castFloatToWord32 f))) TFloat
        -- Runtime float value: box directly
        _ ->
          irBox v TFloat
    TDouble ->
      case v of
        -- Literal double: convert bit pattern to i64, then box
        Double d ->
          irConcealFloat (I64 (fromIntegral (castDoubleToWord64 d))) TDouble
        -- Runtime double value: box directly
        _ ->
          irBox v TDouble
    _ ->
      pure v

irUnbox :: IRValue -> IRType -> IRInstr IRValue
irUnbox v t = do
  p1 <- bitcast v (ptr t)
  load t p1

irConcealArgs :: (IREval e) => NonEmpty e -> IRInstr [IRValue]
irConcealArgs args = do
  vs <- forM args $
    \e -> do
      v1 <- irEval NotInTail e -- Arguments are not in tail position
      v2 <- irConceal v1
      unless (v1 == v2) (irComment ["^ Conceal arg."])
      pure v2
  pure (toList vs)

irReveal :: IRValue -> IRType -> IRInstr IRValue
irReveal v =
  \case
    TInt1 ->
      ptrtoint v i1
    TInt8 ->
      ptrtoint v i8
    TInt32 ->
      ptrtoint v i32
    TInt64 ->
      ptrtoint v i64
    TFloat ->
      irUnbox v TFloat
    TDouble ->
      irUnbox v TDouble
    _ ->
      pure v
