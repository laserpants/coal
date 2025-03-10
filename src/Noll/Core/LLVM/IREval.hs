{-# LANGUAGE LambdaCase #-}

module Noll.Core.LLVM.IREval (
  IREval (..),
  irEvalArgs,
  irEvalFun,
) where

import Control.Monad.Free (Free (..))
import Noll.Common.List1 (List1, fromList1)
import Noll.Core.LLVM.IRInstruction (IRInstr, IRInstrOpF (..))
import Noll.Core.LLVM.IRInstruction.TH (iInttoptr, iRet)
import Noll.Core.LLVM.IRType.Syntax (i8Ptr)
import Noll.Core.LLVM.IRValue (IRValue)

class IREval e where
  irEval :: e -> IRInstr IRValue

{-# INLINE irEvalArgs #-}
irEvalArgs :: (IREval e) => List1 e -> IRInstr [IRValue]
irEvalArgs = mapM irEval . fromList1

{-# INLINE irEvalFun #-}
irEvalFun :: (IREval e) => e -> IRInstr ()
irEvalFun e = simplify (irEval e >>= iRet i8Ptr)

simplify :: IRInstr a -> IRInstr a
simplify =
  \case
    Free (IInttoptr v1 t1 next) ->
      case next v1 of
        Free (IPtrtoint v2 _ next1)
          | v1 == v2 ->
              next1 v1
        Free{} ->
          iInttoptr v1 t1 >>= simplify . next
        _ ->
          iInttoptr v1 t1 >>= next
    Free (MetaBind is i next) ->
      Free (MetaBind is (simplify i) (simplify <$> next))
    Free (MetaBlock name i next) ->
      Free (MetaBlock name (simplify i) (simplify <$> next))
    Free (MetaBlock1 name i next) ->
      Free (MetaBlock1 name (simplify i) (simplify next))
    Free instr ->
      Free (simplify <$> instr)
    i ->
      i
