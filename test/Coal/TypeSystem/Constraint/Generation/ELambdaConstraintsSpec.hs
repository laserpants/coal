{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.Generation.ELambdaConstraintsSpec (spec) where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.TypeSystem.Constraint (Constraint (..))
import Coal.TypeSystem.Constraint.Assumption (Assumption)
import Coal.TypeSystem.Constraint.Generation (emitConstraints, evalConstraintsGenStack)
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Coal.TypeSystem.Constraint.Generation.Stack (ConstraintsGenOutput, emptyConstraintsGenContext)
import Data.Either (rights)
import Data.List.NonEmpty (NonEmpty (..))
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
  ELambda
    ()
    (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
    (EVariable () (Label (TVariable (TypeIndex KType 1)) "x"))

constraint1 :: Constraint (InferenceRule Kind ()) TypeIndex Kind IndexedType
constraint1 =
  Equality
    (RuleAssumption () (TVariable (TypeIndex KType 1)) (TVariable (TypeIndex KType 0)))
    [TVariable (TypeIndex KType 1), TVariable (TypeIndex KType 0)]

fixture2 :: Expression () Kind IndexedType
fixture2 =
  ELambda
    ()
    (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
    (EVariable () (Label (TVariable (TypeIndex KType 1)) "y"))

spec :: Spec
spec = do
  describe "emitELambdaConstraints" $ do
    it "emits an assumption equality for a bound parameter" $ do
      let (ms, outs) = runGen fixture1
      ms `shouldBe` []
      rights outs `shouldSatisfy` (constraint1 `elem`)

    it "computes a fresh id one past the largest used index" $ do
      freshIdIn fixture2 `shouldBe` 2
