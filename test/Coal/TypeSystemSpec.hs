module Coal.TypeSystemSpec (typeSystemSpec) where

import qualified Coal.TypeSystem.Constraint.Generation.EAnnotationConstraintsSpec
import qualified Coal.TypeSystem.Constraint.Generation.EConstructorConstraintsSpec
import qualified Coal.TypeSystem.Constraint.Generation.ELambdaConstraintsSpec
import Coal.TypeSystem.SubstitutionSpec (substitutionSpec)
import Coal.TypeSystem.TypeIndexedSpec (typeIndexedSpec, typeIndexedTrickySpec)
import Coal.TypeSystem.UnificationSpec (unificationSpec)
import Test.Hspec

typeSystemSpec :: Spec
typeSystemSpec =
  describe "TypeSystem" $ do
    unificationSpec
    substitutionSpec
    typeIndexedSpec
    typeIndexedTrickySpec
    describe "Constraint generation" $ do
      Coal.TypeSystem.Constraint.Generation.EAnnotationConstraintsSpec.spec
      Coal.TypeSystem.Constraint.Generation.EConstructorConstraintsSpec.spec
      Coal.TypeSystem.Constraint.Generation.ELambdaConstraintsSpec.spec
