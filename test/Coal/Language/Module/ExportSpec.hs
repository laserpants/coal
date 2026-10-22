{-# LANGUAGE OverloadedStrings #-}

{- | Tests for export-atom name matching.

'Coal.Language.Module.Export.exportsName' decides whether a single export atom
selects a given name.  For a 'TypeExport' the type (or trait / alias) name is
always selected and any listed member is selected as well; an unlisted member
of the same type is not.
-}
module Coal.Language.Module.ExportSpec (exportMatchingSpec) where

import Coal.Language.Module.Export (Export (..), exportsMember, exportsName, includesMember, includesName)
import Test.Hspec (Spec, describe, it, shouldBe)

exportMatchingSpec :: Spec
exportMatchingSpec = do
  describe "exportsName" $ do
    it "selects the exported type name" $
      exportsName "T" (TypeExport () "T" ["A", "B"]) `shouldBe` True

    it "selects a listed constructor" $
      exportsName "A" (TypeExport () "T" ["A", "B"]) `shouldBe` True

    it "does not select an unlisted constructor" $
      exportsName "C" (TypeExport () "T" ["A", "B"]) `shouldBe` False

    it "does not select a constructor when the type exports none" $
      exportsName "A" (TypeExport () "T" []) `shouldBe` False

    it "selects a listed trait member" $
      exportsName "to_int32" (TypeExport () "ToInt32" ["to_int32"]) `shouldBe` True

    it "selects a plain name export" $
      exportsName "foo" (NameExport () "foo") `shouldBe` True

    it "does not select a different name" $
      exportsName "bar" (NameExport () "foo") `shouldBe` False

    it "does not select a value against a type export" $
      exportsName "foo" (TypeExport () "T" ["A"]) `shouldBe` False

  describe "includesName" $ do
    it "selects a name matched by any export in the list" $
      includesName [NameExport () "foo", TypeExport () "T" ["A"]] "A" `shouldBe` True

    it "does not select a name no export matches" $
      includesName [NameExport () "foo", TypeExport () "T" ["A"]] "B" `shouldBe` False

    it "does not select anything from an empty export list" $
      includesName [] "foo" `shouldBe` False

  describe "exportsMember" $ do
    it "selects a listed member of the owning type" $
      exportsMember "T" "A" (TypeExport () "T" ["A", "B"]) `shouldBe` True

    it "does not select an unlisted member of the owning type" $
      exportsMember "T" "C" (TypeExport () "T" ["A", "B"]) `shouldBe` False

    it "does not select a member when the owning type exports none" $
      exportsMember "T" "A" (TypeExport () "T" []) `shouldBe` False

    it "does not select a member listed under a different owner" $
      exportsMember "U" "A" (TypeExport () "T" ["A"]) `shouldBe` False

    -- The pathological case: a type and its single constructor share a name.
    it "does not select a same-named member when the owner exports none" $
      exportsMember "Thing" "Thing" (TypeExport () "Thing" []) `shouldBe` False

    it "selects a same-named member when the owner exports it" $
      exportsMember "Thing" "Thing" (TypeExport () "Thing" ["Thing"]) `shouldBe` True

    it "selects a listed trait member" $
      exportsMember "ToInt32" "to_int32" (TypeExport () "ToInt32" ["to_int32"]) `shouldBe` True

  describe "includesMember" $ do
    it "selects a member matched by the owning type's export" $
      includesMember [TypeExport () "T" ["A"], TypeExport () "U" ["B"]] "T" "A" `shouldBe` True

    it "does not select a member listed under a different type" $
      includesMember [TypeExport () "T" ["A"], TypeExport () "U" ["B"]] "T" "B" `shouldBe` False

    it "does not select anything from an empty export list" $
      includesMember [] "T" "A" `shouldBe` False
