{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.LLVM.IREval.Closure.Call (irCalls, irCallTable, irCallN) where

import Coal.Common.Label (Label (..))
import Coal.LegacyKernel.LLVM.IRConstruct (IRConstruct (..))
import Coal.LegacyKernel.LLVM.IRInstruction (ICmpCond (..), IRInstr)
import Coal.LegacyKernel.LLVM.IRInstruction.Builders
import Coal.LegacyKernel.LLVM.IRInterpreter (interpretFunction)
import Coal.LegacyKernel.LLVM.IRInterpreter.Monad (IRInterpreter, IRLine)
import Coal.LegacyKernel.LLVM.IRType (IRType (..))
import Coal.LegacyKernel.LLVM.IRType.Syntax
import Coal.LegacyKernel.LLVM.IRValue (IRValue (..))
import Extras (forM)
import TextShow (showt)

maxArgs :: Int
maxArgs = 256

irClosureCall :: Int -> IRValue -> IRValue -> IRInstr ()
irClosureCall n argF argAs = do
  r1 <- bitcast argF (opaqueFunction n)
  vs <- forM [0 .. n - 1] $ \m -> do
    r2 <- getelementptr1 i8Ptr argAs (I32 (fromIntegral m))
    load i8Ptr r2
  r4 <- call i8Ptr r1 vs
  ret r4

irCall :: Int -> IRInterpreter (IRConstruct [IRLine])
irCall n =
  interpretFunction
    ("call_" <> showt n)
    (irClosureCall n argF argAs)
    [Label t name | Local t name <- [argF, argAs]]
 where
  argF = Local i8Ptr "f"
  argAs = Local i8PtrPtr "as"

irCalls :: IRInterpreter [IRConstruct [IRLine]]
irCalls = forM [0 .. maxArgs] irCall

irCallTable :: IRConstruct [IRLine]
irCallTable =
  CGlobal
    "call_table"
    (TArray (maxArgs + 1) i8Ptr)
    Nothing
    ( Array
        i8Ptr
        [ Bitcast i8Ptr (fun i8Ptr [i8Ptr, i8PtrPtr]) ("call_" <> showt n) i8Ptr
        | n <- [0 .. maxArgs]
        ]
    )

irClosureCallN :: IRValue -> IRValue -> IRValue -> IRInstr ()
irClosureCallN argF argN argAs = do
  -- Bounds check: ensure argN <= maxArgs
  labelBoundsOk <- label "bounds_ok"
  labelBoundsError <- label "bounds_error"
  r0 <- icmp SGt i1 argN (I32 (fromIntegral maxArgs))
  br r0 [labelBoundsError, labelBoundsOk]
  block1 labelBoundsError $ do
    -- Abort with error message
    _ <- ccall i8Ptr "debug_call_n_bounds" [argN]
    -- Unreachable after abort, but LLVM requires terminator
    ret Null
  block1 labelBoundsOk $ do
    r1 <- getelementptr t (Global (ptr t) "call_table") (I32 0) argN
    r2 <- load i8Ptr r1
    r3 <- bitcast r2 (fun i8Ptr [i8Ptr, i8PtrPtr])
    r4 <- call i8Ptr r3 [argF, argAs]
    ret r4
 where
  t = TArray (maxArgs + 1) i8Ptr

irCallN :: IRInterpreter (IRConstruct [IRLine])
irCallN =
  interpretFunction
    "call_n"
    (irClosureCallN argF argN argAs)
    [Label t name | Local t name <- [argF, argN, argAs]]
 where
  argF = Local i8Ptr "f"
  argN = Local i32 "n"
  argAs = Local i8PtrPtr "as"
