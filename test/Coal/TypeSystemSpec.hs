module Coal.TypeSystemSpec (typeSystemSpec) where

import Coal.TypeSystem.SubstitutionSpec (substitutionSpec)
import Coal.TypeSystem.TypeIndexedSpec
import Coal.TypeSystem.UnificationSpec (unificationSpec)
import Test.Hspec

typeSystemSpec :: Spec
typeSystemSpec =
  describe "TypeSystem" $ do
    unificationSpec
    substitutionSpec
    typeIndexedSpec
    typeIndexedTrickySpec
