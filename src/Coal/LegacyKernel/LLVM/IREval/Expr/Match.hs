{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.LLVM.IREval.Expr.Match (irEvalMatch) where

import Coal.Common.Label (Label (..))
import Coal.LegacyKernel.LLVM.IREval (
  IREval (..),
  IRTailContext (NotInTail),
 )
import Coal.LegacyKernel.LLVM.IREval.Conceal (irReveal)
import Coal.LegacyKernel.LLVM.IRInstruction (IRInstr)
import Coal.LegacyKernel.LLVM.IRInstruction.Builders
import Coal.LegacyKernel.LLVM.IRType (IRType (..), IRTyped (..))
import Coal.LegacyKernel.LLVM.IRType.Syntax (i32, i8Ptr, ptr, struct)
import Coal.LegacyKernel.LLVM.IRValue (IRValue (..))
import qualified Coal.LegacyKernel.Language as Syntax
import Control.Monad.Extra (concatForM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Tuple.Extra (fst3)
import Extras (forM, forSM)

irEvalClause :: (IREval e) => IRTailContext -> IRValue -> [Label Syntax.Type] -> e -> IRInstr IRValue
irEvalClause tailCtx v1 lls e = do
  let t = structType (length lls)
  r1 <- bitcast v1 (ptr t)
  bound <- forSM 1 lls $
    \(Label t1 n) i -> do
      r2 <- getelementptr t r1 (I32 0) (I32 i)
      r3 <- load i8Ptr r2
      r4 <- irReveal r3 (irTypeOf t1)
      pure (n, r4)
  bind bound (irEval tailCtx e)

structType :: Int -> IRType
structType n = struct (i32 : replicate n i8Ptr)

irEvalMatch :: (IREval e, IRTyped t) => IRTailContext -> t -> e -> NonEmpty (Syntax.Clause Syntax.Type e) -> IRInstr IRValue
irEvalMatch tailCtx t e1 cs = do
  v1 <- irEval NotInTail e1 -- Scrutinee not in tail position
  r1 <- bitcast v1 (ptr (struct [i32]))
  r2 <- getelementptr (struct [i32]) r1 (I32 0) (I32 0)
  r3 <- load i32 r2
  labelEnd <- label "end"
  case cs of
    Syntax.Clause (Label{} :| lls) e :| [] ->
      irEvalClause tailCtx v1 lls e -- Single clause inherits tail context
    _ -> do
      ds <- forM (toList cs) $
        \(Syntax.Clause ((Label _ con) :| lls) e) -> do
          n <- label con
          pure (n, lls, e)

      xs <- concatForM (toList cs) $
        \(Syntax.Clause ((Label _ con) :| _) _) -> do
          ix <- constructorLookup con
          case ix of
            Nothing -> do
              pure []
            Just i ->
              pure [i]

      let (n1, _, _) = last ds
      switch r3 n1 (zip (fst3 <$> ds) (I32 . fromIntegral <$> xs))

      bs <- forM ds $
        \(ll, lls, e) ->
          block ll $ do
            r4 <- irEvalClause tailCtx v1 lls e -- All branches inherit tail context
            br1 labelEnd
            pure r4
      (_, v) <-
        block labelEnd $
          phi (irTypeOf t) bs
      pure v
