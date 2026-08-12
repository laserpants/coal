{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.Generation.EConstructorConstraintsSpec (spec) where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.TypeSystem.Constraint.Assumption (Assumption)
import Coal.TypeSystem.Constraint.Generation (emitConstraints, evalConstraintsGenStack)
import Coal.TypeSystem.Constraint.Generation.Error (ConstraintsGenError (..))
import Coal.TypeSystem.Constraint.Generation.Stack (ConstraintsGenOutput, emptyConstraintsGenContext)
import Data.Either (lefts)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

{- | Run constraint generation for an expression, returning the generated
assumptions alongside the raw constraint-generation outputs.
-}
runGen ::
  Expression () Kind IndexedType ->
  ([Assumption () IndexedType], [ConstraintsGenOutput () TypeIndex Kind IndexedType])
runGen expr = evalConstraintsGenStack (freshIdIn expr) emptyConstraintsGenContext (emitConstraints expr)

fixture1 :: Expression () Kind IndexedType
fixture1 =
  EConstructor () (Label (TConstructor KType "Color") "Blue")

spec :: Spec
spec = do
  describe "emitEConstructorConstraints" $ do
    it "reports ENoDataConstructor for an unknown constructor" $ do
      let (ms, outs) = runGen fixture1
      ms `shouldBe` []
      lefts outs `shouldSatisfy` (ENoDataConstructor () "Blue" `elem`)

    it "emits no valid constraints for an unknown constructor" $ do
      let (_, outs) = runGen fixture1
      lefts outs `shouldBe` [ENoDataConstructor () "Blue"]
