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
  iBlock_,
  iDataConstr,
  iHashMapKey,
  iRuntimeApply,
  iRuntimeClosure,
) where

import Control.Monad (void)
import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Noll.Core.LLVM.IRInstruction (IRInstr, IRInstrOp, IRInstrOpF (..))
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Name)

makeFree ''IRInstrOpF

iBlock_ :: (MonadFree IRInstrOp m) => Name -> IRInstr IRValue -> m ()
iBlock_ ll = void . iBlock ll
