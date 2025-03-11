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
  iCmpNe,
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
  iBitcast,
  iPhi,
  iSwitch,
  iComment,
  metaLabel,
  metaIndex,
  metaLookup,
  metaBind,
  metaBlock,
  metaBlock1,
  metaConstructor,
  metaKey,
  metaMemoize,
  metaApply,
  metaClosure,
) where

import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Noll.Core.LLVM.IRInstruction (IRInstrOpF (..))

makeFree ''IRInstrOpF
