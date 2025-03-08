{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Closure.Apply (irClosureApply, irApplyN) where

import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IREval.Comment (irComments)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.Interpreter (
  IRInterpreter (..),
  IRLine,
  interpret,
 )
import Noll.Core.LLVM.IRInstruction.Interpreter.IRConstruct (argLabel, irDefine)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType)
import Noll.Core.LLVM.IRType.Syntax (fun, i32, i8Ptr, i8PtrPtr, ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Label (Label (..))
import Noll.Utils (forM, forM_, listenOnly, second)
import TextShow (showt)

structType :: IRType
structType = struct [i32, i8Ptr, i8Ptr, i8Ptr]

irClosureApply :: Int -> IRValue -> [IRValue] -> IRInstr ()
irClosureApply n argF args = do
  r1 <- iBitcast argF (ptr structType)
  r2 <- iGep structType r1 (I32 0) (I32 0)
  irComments ["Argument count"]
  r3 <- iLoad i32 r2
  r4 <- iGep structType r1 (I32 0) (I32 1)
  irComments ["Finalizer"]
  r5 <- iLoad i8Ptr r4
  labelDefault <- metaLabel "default"
  labels <- forM [1 .. n] $ \m -> do
    ll <- metaLabel ("expects" <> showt m)
    pure (ll, m)
  iSwitch r3 labelDefault (second (I32 . fromIntegral) <$> labels)
  forM_ labels $
    \(ll, m) ->
      if m == n
        then do
          irComments ["Number of supplied arguments (" <> showt n <> ") matches function signature"]
          metaBlock1 ll $ do
            r6 <- iBitcast r5 (fun i8Ptr [i8Ptr, i32, i8PtrPtr])
            r7 <- iAlloca i8Ptr (I32 (fromIntegral m))
            forM_ [0 .. m - 1] $
              \k -> do
                r8 <- iGep1 i8Ptr r7 (I32 (fromIntegral k))
                iStore (args !! k) r8
            r9 <- iCall i8Ptr r6 [argF, I32 (fromIntegral m), r7]
            iRet i8Ptr r9
        else do
          irComments ["Function is oversaturated (+" <> showt (n - m) <> ")"]
          metaBlock1 ll $ do
            r6 <- iBitcast r5 (fun i8Ptr [i8Ptr, i32, i8PtrPtr])
            r7 <- iAlloca i8Ptr (I32 (fromIntegral m))
            forM_ [0 .. m - 1] $
              \k -> do
                r8 <- iGep1 i8Ptr r7 (I32 (fromIntegral k))
                iStore (args !! k) r8
            r9 <- iCall i8Ptr r6 [argF, I32 (fromIntegral m), r7]
            r10 <- iCallGlobal i8Ptr ("apply" <> showt (n - m)) ([r9] <> drop m args)
            iRet i8Ptr r10
  irComments ["Function is undersaturated"]
  metaBlock1 labelDefault $ do
    r10 <- iGep structType r1 (I32 0) (I32 2)
    r11 <- iLoad i8Ptr r10
    r12 <- iBitcast r11 (fun i8Ptr [i8Ptr, i32, i8PtrPtr])
    r13 <- iAlloca i8Ptr (I32 (fromIntegral n))
    forM_ [0 .. n - 1] $
      \m -> do
        r14 <- iGep1 i8Ptr r13 (I32 (fromIntegral m))
        iStore (args !! m) r14
    r15 <- iCall i8Ptr r12 [argF, I32 (fromIntegral n), r13]
    iRet i8Ptr r15

irApplyN :: Int -> IRInterpreter (IRConstruct [IRLine])
irApplyN n =
  irDefine
    ("apply" <> showt n)
    (irClosureApply n arg0 args)
    (argLabel <$> arg0 : args)
 where
  arg0 = Local i8Ptr "f"
  args = Local i8Ptr <$> ["a" <> showt m | m <- [0 .. n - 1]]
