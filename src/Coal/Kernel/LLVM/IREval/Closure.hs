{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.LLVM.IREval.Closure (
  irApplyClosure,
  irPackClosure,
  closureStructType,
) where

import Coal.Kernel.LLVM.IREval
import Coal.Kernel.LLVM.IREval.Comment (irComment)
import Coal.Kernel.LLVM.IREval.Conceal (irConceal, irReveal)
import Coal.Kernel.LLVM.IREval.Malloc (irMalloc)
import Coal.Kernel.LLVM.IRInstruction (IRInstr)
import Coal.Kernel.LLVM.IRInstruction.TH
import Coal.Kernel.LLVM.IRType (IRType (..), IRTyped (..))
import Coal.Kernel.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Coal.Kernel.LLVM.IRValue (IRValue (..))
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Extra (Name, forM, forSM_)
import TextShow (showt)

storeElement :: IRValue -> IRValue -> Int -> IRInstr ()
storeElement base v i = do
  v1 <- getelementptr1 i8Ptr base (I32 (fromIntegral i))
  v2 <- irConceal v
  store v2 v1

irApplyClosure :: (IRTyped t, IREval e) => t -> IRValue -> NonEmpty e -> IRInstr IRValue
irApplyClosure t v es = do
  r1 <- alloca i8Ptr (I32 n)
  vs <- forM es irEval
  forSM_ 0 (toList vs) (storeElement r1)
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
  let t = closureStructType (length vs)
  r1 <- irMalloc (closureStructType (length vs))
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

closureStructType :: Int -> IRType
closureStructType n = struct [i32, i32, i8Ptr, TArray n i8Ptr]

comment1 :: Name -> Int -> Int -> Text
comment1 name args adds =
  showt args
    <> " argument(s) supplied -- target function '"
    <> name
    <> "' expects "
    <> showt adds
    <> " additional argument(s)"
