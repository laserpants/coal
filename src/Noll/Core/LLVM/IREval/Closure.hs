{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Closure (
  irApplyClosure,
  irPackClosure,
  structType,
  namedClosureType,
  maxArgs,
) where

import Data.Text (Text)
import Noll.Common.List1 (List1)
import Noll.Core.LLVM.IREval
import Noll.Core.LLVM.IREval.Comment (irComments)
import Noll.Core.LLVM.IREval.Malloc (irMalloc)
import Noll.Core.LLVM.IRInstruction (IRClosure (..), IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Name, forM_)
import TextShow (showt)

maxArgs :: Int
maxArgs = 32

irApplyClosure :: (IREval e) => IRValue -> List1 e -> IRInstr IRValue
irApplyClosure v es = do
  vs <- irEvalArgs es
  name <- iApply (length es)
  iCallGlobal i8Ptr name (v : vs)

irPackClosure ::
  -- | Target function
  Name ->
  -- | Remaining argument count
  Int ->
  -- | Applied arguments
  [IRValue] ->
  IRInstr IRValue
irPackClosure fname k vs = do
  IRClosure name f1 f2 f3 <- iClosure fname (length vs) k
  let t = TNamed name (structType (length vs))
  r1 <- irMalloc t
  r2 <- iGep t r1 (I32 0) (I32 0)
  irComments [comment fname (length vs) k]
  r3 <- iInttoptr (I32 (fromIntegral k)) i8Ptr
  iStore r3 r2
  r4 <- iGep t r1 (I32 0) (I32 1)
  r5 <- iBitcast f1 i8Ptr
  iStore r5 r4
  r6 <- iGep t r1 (I32 0) (I32 2)
  r7 <- iBitcast f2 i8Ptr
  iStore r7 r6
  r8 <- iGep t r1 (I32 0) (I32 3)
  r9 <- iBitcast f3 i8Ptr
  iStore r9 r8
  forM_ (zip vs [4 ..]) $ \(v, n) -> do
    rn <- iGep t r1 (I32 0) (I32 n)
    iStore v rn
  iBitcast r1 i8Ptr

structType :: Int -> IRType
structType n = struct (i32 : replicate (n + 3) i8Ptr)

namedClosureType :: Int -> IRType
namedClosureType n = TNamed ("closure" <> showt n) (structType n)

comment :: Name -> Int -> Int -> Text
comment name args adds =
  showt args
    <> " argument(s) supplied -- target function '"
    <> name
    <> "' expects "
    <> showt adds
    <> " additional argument(s)"
