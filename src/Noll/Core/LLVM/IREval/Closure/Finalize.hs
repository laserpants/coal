{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Closure.Finalize (
  irClosureFinalize,
  irFinalizeN,
) where

import Control.Monad.State (evalStateT, get, modify)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IREval.Closure (maxArgs, namedClosureType)
import Noll.Core.LLVM.IREval.Comment (irComments)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRInterpreter
import Noll.Core.LLVM.IRInterpreter.Monad
import Noll.Core.LLVM.IRType.Syntax (i1, i32, i8Ptr, i8PtrPtr, opaqueFunction, ptr)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Label (Label (..))
import Noll.Utils (forM, forM_)
import TextShow (showt)

irClosureFinalize :: Int -> IRValue -> IRValue -> IRValue -> IRInstr ()
irClosureFinalize n argF argN argAs = do
  r1 <- iBitcast argF (ptr (namedClosureType n))
  r2 <- iGep (namedClosureType n) r1 (I32 0) (I32 3)
  irComments ["Target function"]
  r3 <- iLoad i8Ptr r2
  rs <- forM [1 .. n] $
    \m -> do
      rm <- iGep (namedClosureType n) r1 (I32 0) (I32 (3 + fromIntegral m))
      irComments ["Applied arg #" <> showt m]
      iLoad i8Ptr rm
  flip evalStateT [] $
    forM_ [1 .. maxArgs - n] $
      \m -> do
        r4 <- iGep1 i8Ptr argAs (I32 (fromIntegral (m - 1)))
        irComments ["Extra arg #" <> showt m]
        r5 <- iLoad i8Ptr r4
        r6 <- iCmpSGt i1 argN (I32 (fromIntegral m))
        labelGt <- metaLabel "gt"
        labelEq <- metaLabel "eq"
        iBr r6 [labelGt, labelEq]
        modify (<> [r5])
        qs <- get
        metaBlock1 labelEq $ do
          r7 <- iBitcast r3 (opaqueFunction (n + m))
          r8 <- iCall i8Ptr r7 (rs <> qs)
          iRet i8Ptr r8
        metaBlock1 labelGt $ pure ()
  iRet i8Ptr Null

irFinalizeN :: Int -> IRInterpreter (IRConstruct [IRLine])
irFinalizeN n = do
  interpretFunction
    ("closure" <> showt n <> "_finalize")
    (irClosureFinalize n arg0 arg1 arg2)
    [Label t name | Local t name <- [arg0, arg1, arg2]]
 where
  arg0 = Local i8Ptr "f"
  arg1 = Local i32 "n"
  arg2 = Local i8PtrPtr "as"
