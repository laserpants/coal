{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval.Closure.Finalize (irClosureFinalize) where

import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (forM_)
import TextShow (showt)

s applied = struct (i32 : replicate (3 + applied) i8Ptr)

xx n = TNamed ("closure" <> showt n) (s n)

irClosureFinalize :: Int -> IRValue -> IRValue -> IRValue -> IRInstr IRValue
irClosureFinalize n argF argN argAs = do
  r1 <- iBCast (Local i8Ptr "f") (ptr (TNamed ("closure" <> showt n) (s n)))
  r2 <- iGep (xx n) r1 (I32 0) (I32 3)
  iComment ""
  iComment "Target function"
  iComment ""
  r3 <- iLoad i8Ptr r2
  forM_ [1 .. n] $ \m -> do
    rm <- iGep (xx n) r1 (I32 0) (I32 (3 + fromIntegral m))
    iComment ""
    iComment ("Applied arg #" <> showt m)
    iComment ""
    iLoad i8Ptr rm
  forM_ [0 .. 3 :: Int] $ \m -> do
    -- ?
    r4 <- iGep1 undefined argAs (I32 (fromIntegral m))
    iComment ""
    iComment ("Extra arg #" <> showt m)
    iComment ""
  pure r3 -- r4
