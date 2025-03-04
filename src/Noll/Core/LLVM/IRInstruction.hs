{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInstruction (
  IRInstrOpF (..),
  IRInstrOp,
  IRInstr,
) where

import Control.Monad.Free (Free)
import Data.Text (Text)
import Noll.Core.LLVM.IRType (IRType)
import Noll.Core.LLVM.IRValue (IRValue)
import Noll.Utils (Name)

-- | LLVM IR language instruction grammar
{- FOURMOLU_DISABLE -}
data IRInstrOpF v t i next
  = IAdd             t v v        (v -> next)
  | ISub             t v v        (v -> next)
  | IMul             t v v        (v -> next)
  | IDiv             t v v        (v -> next)
  | IXOr             t v v        (v -> next)
  | IAnd             t v v        (v -> next)
  | IOr              t v v        (v -> next)
  | ILoad            t v          (v -> next)
  | ICmpEq           t v v        (v -> next)
  | ICmpSLt          t v v        (v -> next)
  | ICmpSGt          t v v        (v -> next)
  | ICmpSLE          t v v        (v -> next)
  | ICmpSGE          t v v        (v -> next)
  | IStore           v v          next
  | IRet             t v          next
  | IBr              v [Name]     next
  | IBr1             Name         next
  | ICall            t v [v]      (v -> next)
  | ICallGlobal      t Name [v]   (v -> next)
  | IGep             t v v v      (v -> next)
  | IGep1            t v v        (v -> next)
  | IGepNull         t v          (v -> next)
  | IInttoptr        v t          (v -> next)
  | IPtrtoint        v t          (v -> next)
  | IAlloca          t            (v -> next)
  | IBCast           v t          (v -> next)
  | IPhi             t [i]        (v -> next)
  | ISwitch          v Name [i]   next
  | IComment         Text         next
  --
  | ILabel           Name         (Name -> next)
  | IIndex                        (Name -> next)
  | ILookup          Name         (v -> next)
  | IBind        [i] (IRInstr v)  (v -> next)
  | IBlock      Name (IRInstr v)  (i -> next)
  | IDataConstr    t Name         ((Int, t) -> next)
  | IHashMapKey      Name         (v -> next)
  | IRuntimeApply    Int          (Name -> next)                -- artifact?
  | IRuntimeClosure  Name Int Int ((Name, v, v, v) -> next)
  -- TODO
  deriving (Functor)
{- FOURMOLU_ENABLE -}

type IRInstrOp = IRInstrOpF IRValue IRType (Name, IRValue)

type IRInstr = Free IRInstrOp
