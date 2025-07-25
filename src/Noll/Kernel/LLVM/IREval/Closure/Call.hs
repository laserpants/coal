{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.LLVM.IREval.Closure.Call (irCalls, irCallTable, irCallN) where

import Noll.Common.Label (Label (..))
import Noll.Kernel.LLVM.IRConstruct (IRConstruct (..))
import Noll.Kernel.LLVM.IRInstruction (IRInstr)
import Noll.Kernel.LLVM.IRInstruction.TH
import Noll.Kernel.LLVM.IRInterpreter (interpretFunction)
import Noll.Kernel.LLVM.IRInterpreter.Monad (IRInterpreter, IRLine)
import Noll.Kernel.LLVM.IRType (IRType (..))
import Noll.Kernel.LLVM.IRType.Syntax
import Noll.Kernel.LLVM.IRValue (IRValue (..))
import Extra (forM)
import TextShow (showt)

maxArgs :: Int
maxArgs = 32 -- TODO: 256 ?

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
