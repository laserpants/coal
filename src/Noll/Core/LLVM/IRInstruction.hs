{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInstruction (IRInstrOp, IRInstr) where

import Control.Monad.Free (Free)
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
  -- TODO
  deriving (Functor)
{- FOURMOLU_ENABLE -}

type IRInstrOp = IRInstrOpF IRValue IRType (Name, IRValue)

type IRInstr = Free IRInstrOp
