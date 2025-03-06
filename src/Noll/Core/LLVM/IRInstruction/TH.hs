{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Noll.Core.LLVM.IRInstruction.TH (
  iAdd,
  iSub,
  iMul,
  iDiv,
  iXOr,
  iAnd,
  iOr,
  iLoad,
  iCmpEq,
  iCmpSLt,
  iCmpSGt,
  iCmpSLE,
  iCmpSGE,
  iStore,
  iRet,
  iBr,
  iBr1,
  iCall,
  iCallGlobal,
  iGep,
  iGep1,
  iGepNull,
  iInttoptr,
  iPtrtoint,
  iAlloca,
  iBCast,
  iPhi,
  iSwitch,
  iComment,
  iLabel,
  iIndex,
  iLookup,
  iBind,
  iBlock,
  iBlock1,
  iDataConstr,
  iHashMapKey,
  iRuntimeApply,
  iRuntimeClosure,
) where

import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Noll.Core.LLVM.IRInstruction (IRInstrOpF (..))

makeFree ''IRInstrOpF
