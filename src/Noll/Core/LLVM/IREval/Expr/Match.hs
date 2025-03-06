{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREval.Expr.Match (irEvalMatch) where

import Data.Tuple.Extra (fst3)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Core.LLVM.IREval
import Noll.Core.LLVM.IREval.Conceal (irConceal)
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, ptr, struct)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Label (Label (..))
import Noll.Utils (forM)

import qualified Noll.Core.Language as Core

irEvalClause :: (IREval e) => IRValue -> [Label Core.Type] -> e -> IRInstr IRValue
irEvalClause v1 lls e = do
  let t = structType (length lls)
  r1 <- iBCast v1 (ptr t)
  bound <- forM (zip lls [1 ..]) $
    \(Label _ n, i) -> do
      r2 <- iGep t r1 (I32 0) (I32 i)
      r3 <- iLoad i8Ptr r2
      pure (n, r3)
  iBind bound (irEval e)

structType :: Int -> IRType
structType n = struct (i32 : replicate n i8Ptr)

irEvalMatch :: (IREval e) => e -> List1 (Core.Clause Core.Type e) -> IRInstr IRValue
irEvalMatch e1 cs = do
  v1 <- irEval e1
  r1 <- iBCast v1 (ptr (struct [i32]))
  r2 <- iGep (struct [i32]) r1 (I32 0) (I32 0)
  r3 <- iLoad i32 r2
  labelEnd <- iLabel "end"
  case cs of
    Core.Clause (Label{} :| lls) e :| [] ->
      irEvalClause v1 lls e
    _ -> do
      ds <- forM cs $
        \(Core.Clause ((Label _ con) :| lls) e) -> do
          n <- iLabel con
          pure (n, lls, e)
      let n :| ns = fst3 <$> ds
      iSwitch r3 n (fromList1 (n :| ns) `zip` (I32 <$> [0 ..]))
      b :| bs <- forM ds $
        \(ll, lls, e) ->
          iBlock ll $ do
            r4 <- irEvalClause v1 lls e
            iBr1 labelEnd
            pure r4
      (_, v) <-
        iBlock labelEnd $
          iPhi (irTypeOf (snd b)) (b : bs)
      irConceal v
