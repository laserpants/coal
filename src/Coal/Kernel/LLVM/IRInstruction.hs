{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.LLVM.IRInstruction (
  InstrOpF (..),
  MetaOpF (..),
  IRInstrOp,
  IRMetaOp,
  IRInstr,
  IRClosure (..),
  IRConstructor (..),
  ICmpCond (..),
  FCmpCond (..),
  TailMarker (..),
) where

import Coal.Kernel.LLVM.IRType (IRType)
import Coal.Kernel.LLVM.IRValue (IRValue)
import Control.Monad.Free (Free)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Extras (Name)

-- | icmp instruction condition codes
data ICmpCond
  = Eq
  | Ne
  | SLt
  | SGt
  | SLte
  | SGte
  deriving (Show, Eq, Ord)

-- | fcmp instruction condition codes
data FCmpCond
  = OEq
  | ONe
  | OLt
  | OGt
  | OLte
  | OGte
  deriving (Show, Eq, Ord)

{- | Tail call markers for LLVM call instructions
NoTail: regular call, Tail: tail call hint, MustTail: required tail call
-}
data TailMarker
  = NoTail
  | Tail
  | MustTail
  deriving (Show, Eq, Ord)

-- | Code generation meta-operations (non-IR helpers)
data MetaOpF v t next
  = MakeLabel Name (Name -> next)
  | MakeIndex (Name -> next)
  | MakeConstructor t Name (IRConstructor t -> next)
  | MakeKey Name (v -> next)
  | MakeString ByteString (v -> next)
  | MakeBignum Integer (v -> next)
  | CCall t Name [v] (v -> next)
  | NameLookup Name (v -> next)
  | CurrentFunction (Maybe Name -> next)
  | ConstructorLookup Name (Maybe Int -> next)
  | Bind [(Name, v)] (IRInstr v) (v -> next)
  | Block Name (IRInstr v) ((Name, v) -> next)
  | Block1 Name (IRInstr ()) next
  | Memoize (v -> next)
  deriving (Functor)

-- | LLVM IR instruction set
data InstrOpF v t next
  = IAdd t v v (v -> next)
  | ISub t v v (v -> next)
  | IMul t v v (v -> next)
  | IFAdd t v v (v -> next)
  | IFSub t v v (v -> next)
  | IFMul t v v (v -> next)
  | IUDiv t v v (v -> next)
  | IFDiv t v v (v -> next)
  | IFNeg t v (v -> next)
  | IXOr t v v (v -> next)
  | IAnd t v v (v -> next)
  | IOr t v v (v -> next)
  | ILoad t v (v -> next)
  | IICmp ICmpCond t v v (v -> next)
  | IFCmp FCmpCond t v v (v -> next)
  | IStore v v next
  | IRet v next
  | ISwitch v Name [(Name, v)] next
  | IBr v [Name] next
  | IBr1 Name next
  | IComment Text next
  | ICall TailMarker t v [v] (v -> next)
  | ICallGlobal TailMarker t Name [v] (v -> next)
  | IGep t v v v (v -> next)
  | IGep1 t v v (v -> next)
  | IGepsize t v (v -> next)
  | IInttoptr v t (v -> next)
  | IPtrtoint v t (v -> next)
  | IZext v t (v -> next)
  | IAlloca t v (v -> next)
  | IAlloca1 t (v -> next)
  | IBitcast v t (v -> next)
  | IPhi t [(Name, v)] (v -> next)
  | IMeta (MetaOpF v t next)
  deriving (Functor)

data IRClosure v = IRClosure Name v v v
  deriving (Show, Eq, Ord)

data IRConstructor t = IRConstructor Int t
  deriving (Show, Eq, Ord)

type IRInstrOp = InstrOpF IRValue IRType

type IRMetaOp = MetaOpF IRValue IRType

type IRInstr = Free IRInstrOp
