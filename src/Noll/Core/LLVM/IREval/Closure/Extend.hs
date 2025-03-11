{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Closure.Extend (
  irClosureExtend,
  irExtendN,
) where

import Control.Monad.State (evalStateT, get, modify)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IREval.Closure (maxArgs, namedClosureType)
import Noll.Core.LLVM.IREval.Comment (irComments)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRInterpreter
import Noll.Core.LLVM.IRType.Syntax (fun, i1, i32, i64, i8Ptr, i8PtrPtr, ptr)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (forM, forM_)
import TextShow (showt)

irClosureExtend :: Int -> IRValue -> IRValue -> IRValue -> IRInstr ()
irClosureExtend n argF argN argAs = do
  r1 <- iBitcast argF (ptr (namedClosureType n))
  r2 <- iGep (namedClosureType n) r1 (I32 0) (I32 0)
  irComments ["Argument count"]
  r3 <- iLoad i32 r2
  irComments ["New argument count"]
  r4 <- iSub i32 r3 argN
  r5 <- iGep (namedClosureType n) r1 (I32 0) (I32 3)
  irComments ["Target function"]
  r6 <- iLoad i8Ptr r5
  rs <- forM [1 .. n] $
    \m -> do
      rm <- iGep (namedClosureType n) r1 (I32 0) (I32 (3 + fromIntegral m))
      irComments ["Applied arg #" <> showt m]
      iLoad i8Ptr rm
  flip evalStateT [] $
    forM_ [1 .. maxArgs - n] $
      \m -> do
        rm <- iGep1 i8Ptr argAs (I32 (fromIntegral (m - 1)))
        irComments ["Extra arg #" <> showt m]
        r7 <- iLoad i8Ptr rm
        r8 <- iCmpSGt i1 argN (I32 (fromIntegral m))
        labelGt <- metaLabel "gt"
        labelEq <- metaLabel "eq"
        iBr r8 [labelGt, labelEq]
        modify (<> [r7])
        qs <- get
        let t = namedClosureType (n + m)
        metaBlock1 labelEq $ do
          r9 <- iGepNull (ptr t) (I32 1)
          r10 <- iPtrtoint r9 i64
          r11 <- iCallGlobal i8Ptr "gc_malloc" [r10]
          r12 <- iBitcast r11 (ptr t)
          r13 <- iGep t r12 (I32 0) (I32 0)
          iStore r4 r13
          r14 <- iGep t r12 (I32 0) (I32 1)
          r15 <- iBitcast (Global (fun i8Ptr [i8Ptr, i32, i8PtrPtr]) ("closure" <> showt (n + m) <> "_finalize")) i8Ptr
          iStore r15 r14
          r16 <- iGep t r12 (I32 0) (I32 2)
          r17 <- iBitcast (Global (fun i8Ptr [i8Ptr, i32, i8PtrPtr]) ("closure" <> showt (n + m) <> "_extend")) i8Ptr
          iStore r17 r16
          r18 <- iGep t r12 (I32 0) (I32 3)
          iStore r6 r18
          forM_ (zip (rs <> qs) [4 ..]) $ \(r, u) -> do
            r19 <- iGep t r12 (I32 0) (I32 u)
            iStore r r19
          iRet i8Ptr r11
        metaBlock1 labelGt $ pure ()
  iRet i8Ptr Null

irExtendN :: Int -> IRInterpreter (IRConstruct [IRLine])
irExtendN n =
  irDefine
    ("closure" <> showt n <> "_extend")
    (irClosureExtend n arg0 arg1 arg2)
    (argLabel <$> [arg0, arg1, arg2])
 where
  arg0 = Local i8Ptr "f"
  arg1 = Local i32 "n"
  arg2 = Local i8PtrPtr "as"
