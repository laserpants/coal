{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Coal.Kernel.LLVM.IRInstruction.TH (
  add,
  sub,
  mul,
  fadd,
  fsub,
  fmul,
  udiv,
  fdiv,
  xor,
  and,
  or,
  load,
  store,
  icmp,
  fcmp,
  ret,
  br,
  br1,
  call,
  callg,
  getelementptr,
  getelementptr1,
  gepsize,
  inttoptr,
  ptrtoint,
  alloca,
  alloca1,
  bitcast,
  phi,
  switch,
  comment,
  ccall,
  label,
  makeIndex,
  nameLookup,
  block,
  block1,
  makeConstructor,
  constructorLookup,
  makeKey,
  makeString,
  makeBignum,
  bind,
  memoize,
) where

import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.Text (Text)
import Coal.Common.Name (Name)
import Coal.Kernel.LLVM.IRInstruction (FCmpCond, ICmpCond, IRInstrOp, InstrOpF (..))
import Coal.Kernel.LLVM.IRType (IRType)
import Coal.Kernel.LLVM.IRValue (IRValue)
import Prelude hiding (and, or)

makeFree ''InstrOpF

{-# INLINE add #-}
add :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
add = iAdd

{-# INLINE sub #-}
sub :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
sub = iSub

{-# INLINE mul #-}
mul :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
mul = iMul

{-# INLINE fadd #-}
fadd :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
fadd = iFAdd

{-# INLINE fsub #-}
fsub :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
fsub = iFSub

{-# INLINE fmul #-}
fmul :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
fmul = iFMul

{-# INLINE udiv #-}
udiv :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
udiv = iUDiv

{-# INLINE fdiv #-}
fdiv :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
fdiv = iFDiv

{-# INLINE xor #-}
xor :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
xor = iXOr

{-# INLINE and #-}
and :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
and = iAnd

{-# INLINE or #-}
or :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
or = iOr

{-# INLINE load #-}
load :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> m IRValue
load = iLoad

{-# INLINE store #-}
store :: (MonadFree (IRInstrOp) m) => IRValue -> IRValue -> m ()
store = iStore

{-# INLINE icmp #-}
icmp :: (MonadFree (IRInstrOp) m) => ICmpCond -> IRType -> IRValue -> IRValue -> m IRValue
icmp = iICmp

{-# INLINE fcmp #-}
fcmp :: (MonadFree (IRInstrOp) m) => FCmpCond -> IRType -> IRValue -> IRValue -> m IRValue
fcmp = iFCmp

{-# INLINE ret #-}
ret :: (MonadFree (IRInstrOp) m) => IRValue -> m ()
ret = iRet

{-# INLINE br #-}
br :: (MonadFree (IRInstrOp) m) => IRValue -> [Name] -> m ()
br = iBr

{-# INLINE br1 #-}
br1 :: (MonadFree (IRInstrOp) m) => Name -> m ()
br1 = iBr1

{-# INLINE call #-}
call :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> [IRValue] -> m IRValue
call = iCall

{-# INLINE callg #-}
callg :: (MonadFree (IRInstrOp) m) => IRType -> Name -> [IRValue] -> m IRValue
callg = iCallGlobal

{-# INLINE ccall #-}
ccall :: (MonadFree (IRInstrOp) m) => IRType -> Name -> [IRValue] -> m IRValue
ccall = cCall

{-# INLINE getelementptr #-}
getelementptr :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> IRValue -> m IRValue
getelementptr = iGep

{-# INLINE getelementptr1 #-}
getelementptr1 :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> IRValue -> m IRValue
getelementptr1 = iGep1

{-# INLINE gepsize #-}
gepsize :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> m IRValue
gepsize = iGepsize

{-# INLINE inttoptr #-}
inttoptr :: (MonadFree (IRInstrOp) m) => IRValue -> IRType -> m IRValue
inttoptr = iInttoptr

{-# INLINE ptrtoint #-}
ptrtoint :: (MonadFree (IRInstrOp) m) => IRValue -> IRType -> m IRValue
ptrtoint = iPtrtoint

{-# INLINE alloca #-}
alloca :: (MonadFree (IRInstrOp) m) => IRType -> IRValue -> m IRValue
alloca = iAlloca

{-# INLINE alloca1 #-}
alloca1 :: (MonadFree (IRInstrOp) m) => IRType -> m IRValue
alloca1 = iAlloca1

{-# INLINE bitcast #-}
bitcast :: (MonadFree (IRInstrOp) m) => IRValue -> IRType -> m IRValue
bitcast = iBitcast

{-# INLINE phi #-}
phi :: (MonadFree (IRInstrOp) m) => IRType -> [(Name, IRValue)] -> m IRValue
phi = iPhi

{-# INLINE switch #-}
switch :: (MonadFree (IRInstrOp) m) => IRValue -> Name -> [(Name, IRValue)] -> m ()
switch = iSwitch

{-# INLINE comment #-}
comment :: (MonadFree (IRInstrOp) m) => Text -> m ()
comment = iComment

{-# INLINE label #-}
label :: (MonadFree (IRInstrOp) m) => Name -> m Name
label = makeLabel
