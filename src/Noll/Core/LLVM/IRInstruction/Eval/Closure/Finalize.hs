{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval.Closure.Finalize (irClosureFinalize) where

import Control.Monad (liftM, void)
import Control.Monad.State (StateT, MonadState, evalStateT, get, modify)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (i1, i32, i8Ptr, ptr, struct, opaqueIRSignature)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (forM, forM_)
import TextShow (showt)

s applied = struct (i32 : replicate (3 + applied) i8Ptr)

xx n = TNamed ("closure" <> showt n) (s n)

irClosureFinalize :: Int -> IRValue -> IRValue -> IRValue -> IRInstr ()
irClosureFinalize n argF argN argAs = do
  r1 <- iBCast (Local i8Ptr "f") (ptr (TNamed ("closure" <> showt n) (s n)))
  r2 <- iGep (xx n) r1 (I32 0) (I32 3)
  iComment ""
  iComment "Target function"
  iComment ""
  r3 <- iLoad i8Ptr r2
  xs <- forM [1 .. n] $ \m -> do
    rm <- iGep (xx n) r1 (I32 0) (I32 (3 + fromIntegral m))
    iComment ""
    iComment ("Applied arg #" <> showt m)
    iComment ""
    iLoad i8Ptr rm
  flip evalStateT [] $ 
    forM_ [1 .. 3 :: Int] $
      \m -> do
        r4 <- iGep1 i8Ptr argAs (I32 (fromIntegral (m - 1)))
        iComment ""
        iComment ("Extra arg #" <> showt m)
        iComment ""
        rx <- iLoad i8Ptr r4
        r5 <- iCmpSGt i1 argN (I32 (fromIntegral m))
        labelGt <- iLabel "gt"
        labelEq <- iLabel "eq"
        iBr r5 [labelGt, labelEq]
        modify (\ys -> ys <> [rx])
        ys <- get
        void $ iBlock labelEq $ do
          r6 <- iBCast r3 (opaqueIRSignature (n + m))
          r7 <- iCall i8Ptr r6 (xs <> ys)
          iRet i8Ptr r7
          pure r7
        void $ iBlock labelGt $ do
          pure r5
        pure r5
  iRet i8Ptr Null
