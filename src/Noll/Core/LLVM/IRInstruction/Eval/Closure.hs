{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval.Closure (irPackClosure) where

import Data.Text (Text)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.Eval.Malloc (irMalloc)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Name, forM_)
import TextShow (showt)

irPackClosure :: Name -> Int -> [IRValue] -> IRInstr IRValue
irPackClosure fname a vs = do
  (name, f1, f2, f3) <- iRuntimeClosure fname (length vs) a
  let t = TNamed name (structType (length vs))
  r1 <- irMalloc t
  r2 <- iGep t r1 (I32 0) (I32 0)
  iComment ""
  iComment (comment fname (length vs) a)
  iComment ""
  r3 <- iInttoptr (I32 (fromIntegral a)) i8Ptr
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
    rn <- iGep t r1 (I32 0) (I32 n)
    iStore v rn
  iBCast r1 i8Ptr

structType :: Int -> IRType
structType n = struct (i32 : replicate (n + 3) i8Ptr)

comment :: Name -> Int -> Int -> Text
comment name args adds =
  showt args
    <> " argument(s) supplied -- target function '"
    <> name
    <> "' expects "
    <> showt adds
    <> " additional argument(s)"
