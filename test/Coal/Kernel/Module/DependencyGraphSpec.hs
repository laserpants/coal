{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Module.DependencyGraphSpec (spec) where

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Module.DependencyGraph (checkImportsSatisfied, topoSortModules)
import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Minimal module with no objects.
mkModule :: Name -> [Name] -> Module Type
mkModule name imports = Module{moduleName = name, moduleImports = imports, moduleObjects = []}

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "topoSortModules" $ do
    it "returns Right [m] for a single module with no imports" $ do
      let m = mkModule "M" []
      topoSortModules [m] `shouldBe` Right [m]

    it "sorts a linear chain A→B→C into [C, B, A] regardless of input order" $ do
      let a = mkModule "A" ["B"]
          b = mkModule "B" ["C"]
          c = mkModule "C" []
          expectNames = ["C", "B", "A"]
      fmap (map moduleName) (topoSortModules [a, b, c]) `shouldBe` Right expectNames
      fmap (map moduleName) (topoSortModules [c, a, b]) `shouldBe` Right expectNames
      fmap (map moduleName) (topoSortModules [b, c, a]) `shouldBe` Right expectNames

    it "detects a two-module cycle" $ do
      let a = mkModule "A" ["B"]
          b = mkModule "B" ["A"]
      case topoSortModules [a, b] of
        Left names -> names `shouldMatchList` ["A", "B"]
        Right _ -> fail "Expected Left for a cycle, got Right"

    it "detects a self-import" $ do
      let a = mkModule "A" ["A"]
      case topoSortModules [a] of
        Left names -> names `shouldMatchList` ["A"]
        Right _ -> fail "Expected Left for self-import, got Right"

  describe "checkImportsSatisfied" $ do
    it "returns [] when all imports are present" $ do
      let a = mkModule "A" ["B"]
          b = mkModule "B" []
      checkImportsSatisfied [a, b] `shouldBe` []

    it "returns [] for a module with no imports" $ do
      checkImportsSatisfied [mkModule "M" []] `shouldBe` []

    it "returns [(importer, missing)] for an absent import" $ do
      let a = mkModule "A" ["X"]
      checkImportsSatisfied [a] `shouldBe` [("A", "X")]

    it "reports missing imports from multiple modules" $ do
      let a = mkModule "A" ["X"]
          b = mkModule "B" ["Y"]
      checkImportsSatisfied [a, b] `shouldMatchList` [("A", "X"), ("B", "Y")]
