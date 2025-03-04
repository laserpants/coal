{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval.Closure (irPackClosure) where

import Debug.Trace
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.Eval.Malloc (irMalloc)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Name, forM_)
import TextShow (showt)

irClosureType :: Int -> IRType
irClosureType n = struct $ [i32] <> replicate (n + 3) i8Ptr

irPackClosure :: Name -> Int -> [IRValue] -> IRInstr IRValue
irPackClosure fn m vs = do
  (name, f1, f2, f3) <- iRuntimeClosure fn (length vs) m
  let t = TNamed name (irClosureType (length vs))
  r1 <- irMalloc t
  r2 <- iGep t r1 (I32 0) (I32 0)
  iComment ""
  iComment (showt (length vs) <> " argument(s) supplied -- target function '" <> fn <> "' expects " <> showt m <> " additional argument(s)")
  iComment ""
  r3 <- iInttoptr (I32 (fromIntegral m)) i8Ptr
  iStore r3 r2
  r4 <- iGep t r1 (I32 0) (I32 1)
  r5 <- iBCast f1 i8Ptr
  iStore r5 r4
  r6 <- iGep t r1 (I32 0) (I32 2)
  r7 <- iBCast f2 i8Ptr
  iStore r7 r6
  r8 <- iGep t r1 (I32 0) (I32 3)
  r9 <- iBCast f3 i8Ptr
  iStore r9 r8
  forM_ (zip vs [4 ..]) $ \(v, n) -> do
    qn <- iGep t r1 (I32 0) (I32 n)
    iStore v qn
  iBCast r1 i8Ptr
