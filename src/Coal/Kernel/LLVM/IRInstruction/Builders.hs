{-# LANGUAGE FlexibleContexts #-}

module Coal.Kernel.LLVM.IRInstruction.Builders (
  add,
  sub,
  mul,
  fadd,
  fsub,
  fmul,
  udiv,
  fdiv,
  fneg,
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
  tailCall,
  tailCallg,
  musttailCall,
  musttailCallg,
  getelementptr,
  getelementptr1,
  gepsize,
  inttoptr,
  ptrtoint,
  zext,
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
  currentFunction,
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

import Coal.Common.Name (Name)
import Coal.Kernel.LLVM.IRInstruction (FCmpCond, ICmpCond, IRConstructor, IRInstr, IRInstrOp, InstrOpF (..), MetaOpF (..), TailMarker (..))
import Coal.Kernel.LLVM.IRType (IRType)
import Coal.Kernel.LLVM.IRValue (IRValue)
import Control.Monad.Free (MonadFree, liftF)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Prelude hiding (and, or)

{-# INLINE add #-}
add :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
add t v1 v2 = liftF (IAdd t v1 v2 id)

{-# INLINE sub #-}
sub :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
sub t v1 v2 = liftF (ISub t v1 v2 id)

{-# INLINE mul #-}
mul :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
mul t v1 v2 = liftF (IMul t v1 v2 id)

{-# INLINE fadd #-}
fadd :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
fadd t v1 v2 = liftF (IFAdd t v1 v2 id)

{-# INLINE fsub #-}
fsub :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
fsub t v1 v2 = liftF (IFSub t v1 v2 id)

{-# INLINE fmul #-}
fmul :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
fmul t v1 v2 = liftF (IFMul t v1 v2 id)

{-# INLINE udiv #-}
udiv :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
udiv t v1 v2 = liftF (IUDiv t v1 v2 id)

{-# INLINE fdiv #-}
fdiv :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
fdiv t v1 v2 = liftF (IFDiv t v1 v2 id)

{-# INLINE fneg #-}
fneg :: (MonadFree IRInstrOp m) => IRType -> IRValue -> m IRValue
fneg t v = liftF (IFNeg t v id)

{-# INLINE xor #-}
xor :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
xor t v1 v2 = liftF (IXOr t v1 v2 id)

{-# INLINE and #-}
and :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
and t v1 v2 = liftF (IAnd t v1 v2 id)

{-# INLINE or #-}
or :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
or t v1 v2 = liftF (IOr t v1 v2 id)

{-# INLINE load #-}
load :: (MonadFree IRInstrOp m) => IRType -> IRValue -> m IRValue
load t v = liftF (ILoad t v id)

{-# INLINE store #-}
store :: (MonadFree IRInstrOp m) => IRValue -> IRValue -> m ()
store v1 v2 = liftF (IStore v1 v2 ())

{-# INLINE icmp #-}
icmp :: (MonadFree IRInstrOp m) => ICmpCond -> IRType -> IRValue -> IRValue -> m IRValue
icmp cond t v1 v2 = liftF (IICmp cond t v1 v2 id)

{-# INLINE fcmp #-}
fcmp :: (MonadFree IRInstrOp m) => FCmpCond -> IRType -> IRValue -> IRValue -> m IRValue
fcmp cond t v1 v2 = liftF (IFCmp cond t v1 v2 id)

{-# INLINE ret #-}
ret :: (MonadFree IRInstrOp m) => IRValue -> m ()
ret v = liftF (IRet v ())

{-# INLINE br #-}
br :: (MonadFree IRInstrOp m) => IRValue -> [Name] -> m ()
br v names = liftF (IBr v names ())

{-# INLINE br1 #-}
br1 :: (MonadFree IRInstrOp m) => Name -> m ()
br1 name = liftF (IBr1 name ())

{-# INLINE call #-}
call :: (MonadFree IRInstrOp m) => IRType -> IRValue -> [IRValue] -> m IRValue
call t v vs = liftF (ICall NoTail t v vs id)

{-# INLINE callg #-}
callg :: (MonadFree IRInstrOp m) => IRType -> Name -> [IRValue] -> m IRValue
callg t name vs = liftF (ICallGlobal NoTail t name vs id)

{-# INLINE tailCall #-}
tailCall :: (MonadFree IRInstrOp m) => IRType -> IRValue -> [IRValue] -> m IRValue
tailCall t v vs = liftF (ICall Tail t v vs id)

{-# INLINE tailCallg #-}
tailCallg :: (MonadFree IRInstrOp m) => IRType -> Name -> [IRValue] -> m IRValue
tailCallg t name vs = liftF (ICallGlobal Tail t name vs id)

{-# INLINE musttailCall #-}
musttailCall :: (MonadFree IRInstrOp m) => IRType -> IRValue -> [IRValue] -> m IRValue
musttailCall t v vs = liftF (ICall MustTail t v vs id)

{-# INLINE musttailCallg #-}
musttailCallg :: (MonadFree IRInstrOp m) => IRType -> Name -> [IRValue] -> m IRValue
musttailCallg t name vs = liftF (ICallGlobal MustTail t name vs id)

{-# INLINE getelementptr #-}
getelementptr :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> IRValue -> m IRValue
getelementptr t v1 v2 v3 = liftF (IGep t v1 v2 v3 id)

{-# INLINE getelementptr1 #-}
getelementptr1 :: (MonadFree IRInstrOp m) => IRType -> IRValue -> IRValue -> m IRValue
getelementptr1 t v1 v2 = liftF (IGep1 t v1 v2 id)

{-# INLINE gepsize #-}
gepsize :: (MonadFree IRInstrOp m) => IRType -> IRValue -> m IRValue
gepsize t v = liftF (IGepsize t v id)

{-# INLINE inttoptr #-}
inttoptr :: (MonadFree IRInstrOp m) => IRValue -> IRType -> m IRValue
inttoptr v t = liftF (IInttoptr v t id)

{-# INLINE ptrtoint #-}
ptrtoint :: (MonadFree IRInstrOp m) => IRValue -> IRType -> m IRValue
ptrtoint v t = liftF (IPtrtoint v t id)

{-# INLINE zext #-}
zext :: (MonadFree IRInstrOp m) => IRValue -> IRType -> m IRValue
zext v t = liftF (IZext v t id)

{-# INLINE alloca #-}
alloca :: (MonadFree IRInstrOp m) => IRType -> IRValue -> m IRValue
alloca t v = liftF (IAlloca t v id)

{-# INLINE alloca1 #-}
alloca1 :: (MonadFree IRInstrOp m) => IRType -> m IRValue
alloca1 t = liftF (IAlloca1 t id)

{-# INLINE bitcast #-}
bitcast :: (MonadFree IRInstrOp m) => IRValue -> IRType -> m IRValue
bitcast v t = liftF (IBitcast v t id)

{-# INLINE phi #-}
phi :: (MonadFree IRInstrOp m) => IRType -> [(Name, IRValue)] -> m IRValue
phi t vs = liftF (IPhi t vs id)

{-# INLINE switch #-}
switch :: (MonadFree IRInstrOp m) => IRValue -> Name -> [(Name, IRValue)] -> m ()
switch v name cases = liftF (ISwitch v name cases ())

{-# INLINE comment #-}
comment :: (MonadFree IRInstrOp m) => Text -> m ()
comment text = liftF (IComment text ())

-- Meta-operation smart constructors (manually defined to wrap in IMeta)

{-# INLINE ccall #-}
ccall :: (MonadFree IRInstrOp m) => IRType -> Name -> [IRValue] -> m IRValue
ccall t name vs = liftF (IMeta (CCall t name vs id))

{-# INLINE label #-}
label :: (MonadFree (IRInstrOp) m) => Name -> m Name
label name = liftF (IMeta (MakeLabel name id))

{-# INLINE makeIndex #-}
makeIndex :: (MonadFree (IRInstrOp) m) => m Name
makeIndex = liftF (IMeta (MakeIndex id))

{-# INLINE makeConstructor #-}
makeConstructor :: (MonadFree (IRInstrOp) m) => IRType -> Name -> m (IRConstructor IRType)
makeConstructor t name = liftF (IMeta (MakeConstructor t name id))

{-# INLINE makeKey #-}
makeKey :: (MonadFree (IRInstrOp) m) => Name -> m IRValue
makeKey name = liftF (IMeta (MakeKey name id))

{-# INLINE makeString #-}
makeString :: (MonadFree (IRInstrOp) m) => ByteString -> m IRValue
makeString str = liftF (IMeta (MakeString str id))

{-# INLINE makeBignum #-}
makeBignum :: (MonadFree (IRInstrOp) m) => Integer -> m IRValue
makeBignum n = liftF (IMeta (MakeBignum n id))

{-# INLINE nameLookup #-}
nameLookup :: (MonadFree (IRInstrOp) m) => Name -> m IRValue
nameLookup name = liftF (IMeta (NameLookup name id))

{-# INLINE currentFunction #-}
currentFunction :: (MonadFree (IRInstrOp) m) => m (Maybe Name)
currentFunction = liftF (IMeta (CurrentFunction id))

{-# INLINE constructorLookup #-}
constructorLookup :: (MonadFree (IRInstrOp) m) => Name -> m (Maybe Int)
constructorLookup name = liftF (IMeta (ConstructorLookup name id))

{-# INLINE bind #-}
bind :: (MonadFree IRInstrOp m) => [(Name, IRValue)] -> IRInstr IRValue -> m IRValue
bind bindings instr = liftF (IMeta (Bind bindings instr id))

{-# INLINE block #-}
block :: (MonadFree IRInstrOp m) => Name -> IRInstr IRValue -> m (Name, IRValue)
block name instr = liftF (IMeta (Block name instr id))

{-# INLINE block1 #-}
block1 :: (MonadFree IRInstrOp m) => Name -> IRInstr () -> m ()
block1 name instr = liftF (IMeta (Block1 name instr ()))

{-# INLINE memoize #-}
memoize :: (MonadFree (IRInstrOp) m) => m IRValue
memoize = liftF (IMeta (Memoize id))
