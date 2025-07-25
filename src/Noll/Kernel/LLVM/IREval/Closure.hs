{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.LLVM.IREval.Closure (
  irApplyClosure,
  irPackClosure,
  closureStructType,
) where

import Control.Monad (unless)
import Data.Text (Text)
import Extra (Name, forM, forSM_)
import Noll.Common.List1 (List1, fromList1)
import Noll.Kernel.LLVM.IREval
import Noll.Kernel.LLVM.IREval.Comment (irComment)
import Noll.Kernel.LLVM.IREval.Conceal (irConceal, irReveal)
import Noll.Kernel.LLVM.IREval.Malloc (irMalloc)
import Noll.Kernel.LLVM.IRInstruction (IRInstr)
import Noll.Kernel.LLVM.IRInstruction.TH
import Noll.Kernel.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Kernel.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Noll.Kernel.LLVM.IRValue (IRValue (..))
import TextShow (showt)

storeElement :: IRValue -> IRValue -> Int -> IRInstr ()
storeElement base v i = do
  v1 <- getelementptr1 i8Ptr base (I32 (fromIntegral i))
  v2 <- irConceal v
  store v2 v1

irApplyClosure :: (IRTyped t, IREval e) => t -> IRValue -> List1 e -> IRInstr IRValue
irApplyClosure t v es = do
  r1 <- alloca i8Ptr (I32 n)
  vs <- forM es irEval
  forSM_ 0 (fromList1 vs) (storeElement r1)
  r2 <- callg i8Ptr "apply" [v, I32 n, r1]
  irReveal r2 (irTypeOf t)
 where
  n = fromIntegral (length es)

irPackClosure ::
  -- | Target function
  Name ->
  -- | Remaining argument count
  Int ->
  -- | Applied arguments
  [IRValue] ->
  IRInstr IRValue
irPackClosure name k vs = do
  let t = TNamed "closure" closureStructType
  r1 <- irMalloc t
  r2 <- getelementptr t r1 (I32 0) (I32 0)
  store (I32 (fromIntegral (length vs))) r2
  r3 <- getelementptr t r1 (I32 0) (I32 1)
  irComment [comment1 name (length vs) k]
  store (I32 (fromIntegral k)) r3
  r4 <- getelementptr t r1 (I32 0) (I32 2)
  v1 <- nameLookup name
  r5 <- bitcast v1 i8Ptr
  store r5 r4
  unless (null vs) $ do
    base <- getelementptr t r1 (I32 0) (I32 3)
    forSM_ 0 vs (storeElement base)
  bitcast r1 i8Ptr

-- TODO:
-- =====
-- The last i8Ptr shouldn't be necessary, but removing causes a segfault
-- This needs to be investigated further to locate where this issue occurs.
closureStructType :: IRType
closureStructType = struct [i32, i32, i8Ptr, i8Ptr, i8Ptr]

comment1 :: Name -> Int -> Int -> Text
comment1 name args adds =
  showt args
    <> " argument(s) supplied -- target function '"
    <> name
    <> "' expects "
    <> showt adds
    <> " additional argument(s)"
