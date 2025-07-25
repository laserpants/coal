{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.LLVM.IREval.Expr.Match (irEvalMatch) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..), fromList1)
import Coal.Kernel.LLVM.IREval
import Coal.Kernel.LLVM.IREval.Conceal (irReveal)
import Coal.Kernel.LLVM.IRInstruction (IRInstr)
import Coal.Kernel.LLVM.IRInstruction.TH
import Coal.Kernel.LLVM.IRType (IRType (..), IRTyped (..))
import Coal.Kernel.LLVM.IRType.Syntax (i32, i8Ptr, ptr, struct)
import Coal.Kernel.LLVM.IRValue (IRValue (..))
import Data.Tuple.Extra (fst3)
import Extra (forM, forSM, (<$$>))

import qualified Coal.Kernel.Language as Core

irEvalClause :: (IREval e) => IRValue -> [Label Core.Type] -> e -> IRInstr IRValue
irEvalClause v1 lls e = do
  let t = structType (length lls)
  r1 <- bitcast v1 (ptr t)
  bound <- forSM 1 lls $
    \(Label t1 n) i -> do
      r2 <- getelementptr t r1 (I32 0) (I32 i)
      r3 <- load i8Ptr r2
      r4 <- irReveal r3 (irTypeOf t1)
      pure (n, r4)
  bind bound (irEval e)

structType :: Int -> IRType
structType n = struct (i32 : replicate n i8Ptr)

irEvalMatch :: (IREval e, IRTyped t) => t -> e -> List1 (Core.Clause Core.Type e) -> IRInstr IRValue
irEvalMatch t e1 cs = do
  v1 <- irEval e1
  r1 <- bitcast v1 (ptr (struct [i32]))
  r2 <- getelementptr (struct [i32]) r1 (I32 0) (I32 0)
  r3 <- load i32 r2
  labelEnd <- label "end"
  case cs of
    Core.Clause (Label{} :| lls) e :| [] ->
      irEvalClause v1 lls e
    _ -> do
      ds <- forM (fromList1 cs) $
        \(Core.Clause ((Label _ con) :| lls) e) -> do
          n <- label con
          pure (n, lls, e)

      xs <- concat <$$> forM (fromList1 cs) $
        \(Core.Clause ((Label _ con) :| _) _) -> do
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
            r4 <- irEvalClause v1 lls e
            br1 labelEnd
            pure r4
      (_, v) <-
        block labelEnd $
          phi (irTypeOf t) bs
      pure v
