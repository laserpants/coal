{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Closure.Finalize (irClosureFinalize) where

import Control.Monad.State (evalStateT, get, modify)
import Noll.Core.LLVM.IREval.Closure (namedClosureType)
import Noll.Core.LLVM.IREval.Comment (irComments)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType.Syntax (i1, i8Ptr, opaqueIRSignature, ptr)
import Noll.Core.LLVM.IRValue (IRValue (..))
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
    forM_ [1 .. 3] $
      \m -> do
        r4 <- iGep1 i8Ptr argAs (I32 (fromIntegral (m - 1)))
        irComments ["Extra arg #" <> showt m]
        r5 <- iLoad i8Ptr r4
        r6 <- iCmpSGt i1 argN (I32 (fromIntegral m))
        labelGt <- iLabel "gt"
        labelEq <- iLabel "eq"
        iBr r6 [labelGt, labelEq]
        modify (<> [r5])
        qs <- get
        iBlock1 labelEq $ do
          r7 <- iBitcast r3 (opaqueIRSignature (n + m))
          r8 <- iCall i8Ptr r7 (rs <> qs)
          iRet i8Ptr r8
        iBlock1 labelGt $ pure ()
  iRet i8Ptr Null
