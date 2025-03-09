{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInstruction (
  IRInstrOpF (..),
  IRInstrOp,
  IRInstr,
  IRClosure (..),
  IRConstructor (..),
) where

import Control.Monad.Free (Free)
import Data.Text (Text)
import Noll.Core.LLVM.IRType (IRType)
import Noll.Core.LLVM.IRValue (IRValue)
import Noll.Utils (Name)

-- | LLVM IR language instruction grammar
data IRInstrOpF v t i next
  = IAdd t v v (v -> next)
  | ISub t v v (v -> next)
  | IMul t v v (v -> next)
  | IDiv t v v (v -> next)
  | IXOr t v v (v -> next)
  | IAnd t v v (v -> next)
  | IOr t v v (v -> next)
  | ILoad t v (v -> next)
  | ICmpEq t v v (v -> next)
  | ICmpSLt t v v (v -> next)
  | ICmpSGt t v v (v -> next)
  | ICmpSLE t v v (v -> next)
  | ICmpSGE t v v (v -> next)
  | IStore v v next
  | IRet t v next
  | ISwitch v Name [i] next
  | IBr v [Name] next
  | IBr1 Name next
  | IComment Text next
  | ICall t v [v] (v -> next)
  | ICallGlobal t Name [v] (v -> next)
  | IGep t v v v (v -> next)
  | IGep1 t v v (v -> next)
  | IGepNull t v (v -> next)
  | IInttoptr v t (v -> next)
  | IPtrtoint v t (v -> next)
  | IAlloca t v (v -> next)
  | IBitcast v t (v -> next)
  | IPhi t [i] (v -> next)
  | MetaLookup Name (v -> next)
  | MetaBind [i] (IRInstr v) (v -> next)
  | MetaLabel Name (Name -> next)
  | MetaIndex (Name -> next)
  | MetaBlock Name (IRInstr v) (i -> next)
  | MetaBlock1 Name (IRInstr ()) next
  | MetaConstructor t Name (IRConstructor t -> next)
  | MetaKey Name (v -> next)
  | MetaMemoize (v -> next)
  | MetaApply Int (Name -> next)
  | MetaClosure Name Int Int (IRClosure v -> next)
  deriving (Functor)

data IRClosure v = IRClosure Name v v v
  deriving (Show, Eq, Ord)

data IRConstructor t = IRConstructor Int t
  deriving (Show, Eq, Ord)

type IRInstrOp = IRInstrOpF IRValue IRType (Name, IRValue)

type IRInstr = Free IRInstrOp
