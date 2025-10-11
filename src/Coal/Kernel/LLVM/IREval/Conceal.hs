{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.LLVM.IREval.Conceal (irConceal, irConcealArgs, irReveal) where

import Coal.Kernel.LLVM.IREval (IREval (..))
import Coal.Kernel.LLVM.IREval.Comment (irComment)
import Coal.Kernel.LLVM.IREval.Malloc (irMalloc)
import Coal.Kernel.LLVM.IRInstruction (IRInstr)
import Coal.Kernel.LLVM.IRInstruction.TH
import Coal.Kernel.LLVM.IRType (IRType (..), IRTyped (..))
import Coal.Kernel.LLVM.IRType.Syntax (i1, i32, i64, i8, i8Ptr, ptr)
import Coal.Kernel.LLVM.IRValue (IRValue (..))
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty, toList)
import Extra (forM)
import GHC.Float

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
        Float f ->
          irConcealFloat (I32 (fromIntegral (castFloatToWord32 f))) TFloat
        _ ->
          irBox v TFloat
    TDouble ->
      case v of
        Double d ->
          irConcealFloat (I64 (fromIntegral (castDoubleToWord64 d))) TDouble
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
      v1 <- irEval e
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
