{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.Generation.EAnnotationConstraintsSpec (spec) where

import Coal.Language
import Coal.TypeSystem.Constraint (Constraint (..))
import Coal.TypeSystem.Constraint.Assumption (Assumption)
import Coal.TypeSystem.Constraint.Generation (emitConstraints, evalConstraintsGenStack)
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Coal.TypeSystem.Constraint.Generation.Stack (ConstraintsGenOutput, emptyConstraintsGenContext)
import Data.Either (lefts, rights)
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
  EAnnotation () (TIntrinsic IInt32) (ELiteral () (LInt32 1))

constraint1 :: Constraint (InferenceRule Kind ()) TypeIndex Kind IndexedType
constraint1 =
  Equality
    (RuleAnnotation () (TIntrinsic IInt32) (TIntrinsic IInt32))
    [TIntrinsic IInt32, TIntrinsic IInt32]

fixture2 :: Expression () Kind IndexedType
fixture2 =
  EAnnotation () (TIntrinsic IBool) (ELiteral () (LInt32 1))

constraint2 :: Constraint (InferenceRule Kind ()) TypeIndex Kind IndexedType
constraint2 =
  Equality
    (RuleAnnotation () (TIntrinsic IInt32) (TIntrinsic IBool))
    [TIntrinsic IInt32, TIntrinsic IBool]

fixture3 :: Expression () Kind IndexedType
fixture3 =
  EAnnotation () (TVariable (Parameter KType "a")) (ELiteral () (LInt32 1))

spec :: Spec
spec = do
  describe "emitEAnnotationConstraints" $ do
    it "emits an equality constraint between the annotated and literal types" $ do
      let (ms, outs) = runGen fixture1
      ms `shouldBe` []
      rights outs `shouldBe` [constraint1]

    it "emits an equality constraint for a mismatching annotation" $ do
      let (ms, outs) = runGen fixture2
      ms `shouldBe` []
      rights outs `shouldBe` [constraint2]

    it "emits no errors when the annotation is a type variable" $ do
      let (_, outs) = runGen fixture3
      lefts outs `shouldSatisfy` null
