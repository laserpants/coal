{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the surface module header parser.

These pin the export / import member-list conventions, which are deliberately
symmetric:

@
T(A, B)  -- the type together with the listed members
T()      -- the type with none of its members
T        -- the type with all of its members (the wildcard @["*"]@)
@
-}
module Coal.Parser.ModuleSpec (moduleHeaderSpec) where

import Coal.Language.Definition (Definition (..))
import Coal.Language.Module (ExportList (..), Module (..))
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Import (Import (..))
import Coal.Parser (parseSourceFile)
import Data.Text (Text)
import Extras (Name)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (runParser)

{- | A metadata-free view of an export list, so that expectations do not depend
on source positions.
-}
data SimpleExportList
  = SimpleAll
  | SimpleExports [SimpleExport]
  deriving (Eq, Show)

data SimpleExport
  = SimpleName Name
  | SimpleType Name [Name]
  deriving (Eq, Show)

-- | A metadata-free view of a module's imports.
data SimpleImport
  = SimpleNameImport Name
  | SimpleTypeImport Name [Name]
  deriving (Eq, Show)

exportsOf :: Text -> Either String SimpleExportList
exportsOf src =
  case runParser parseSourceFile "" src of
    Left err ->
      Left (show err)
    Right Module{moduleExportList} ->
      Right $
        case moduleExportList of
          ExportAll ->
            SimpleAll
          Exports exports ->
            SimpleExports (simplifyExport <$> exports)

simplifyExport :: Export a -> SimpleExport
simplifyExport = \case
  NameExport _ name ->
    SimpleName name
  TypeExport _ name members ->
    SimpleType name members

importsOf :: Text -> Either String [SimpleImport]
importsOf src =
  case runParser parseSourceFile "" src of
    Left err ->
      Left (show err)
    Right Module{moduleDefinitions} ->
      Right (concatMap collect moduleDefinitions)
 where
  collect = \case
    DImport _ _ imports -> simplifyImport <$> imports
    _ -> []
  simplifyImport = \case
    NameImport _ name -> SimpleNameImport name
    TypeImport _ name members -> SimpleTypeImport name members

moduleHeaderSpec :: Spec
moduleHeaderSpec = do
  describe "module export lists" $ do
    it "exports everything when the list is omitted" $
      exportsOf "module M { }" `shouldBe` Right SimpleAll

    it "exports nothing when the list is empty" $
      exportsOf "module M() { }" `shouldBe` Right (SimpleExports [])

    it "treats a bare type name as the wildcard" $
      exportsOf "module M(T) { }"
        `shouldBe` Right (SimpleExports [SimpleType "T" ["*"]])

    it "treats an empty member list as no members" $
      exportsOf "module M(T()) { }"
        `shouldBe` Right (SimpleExports [SimpleType "T" []])

    it "accepts a single constructor" $
      exportsOf "module M(T(A)) { }"
        `shouldBe` Right (SimpleExports [SimpleType "T" ["A"]])

    it "accepts several constructors" $
      exportsOf "module M(T(A, B)) { }"
        `shouldBe` Right (SimpleExports [SimpleType "T" ["A", "B"]])

    it "accepts trait member names" $
      exportsOf "module M(ToInt32(to_int32)) { }"
        `shouldBe` Right (SimpleExports [SimpleType "ToInt32" ["to_int32"]])

    it "accepts a mixed list of type and name exports" $
      exportsOf "module M(T(A), foo, U) { }"
        `shouldBe` Right (SimpleExports [SimpleType "T" ["A"], SimpleName "foo", SimpleType "U" ["*"]])

  describe "module import lists" $ do
    it "treats a bare type name as the wildcard" $
      importsOf "module M { import N(T) }"
        `shouldBe` Right [SimpleTypeImport "T" ["*"]]

    it "treats an empty member list as no members" $
      importsOf "module M { import N(T()) }"
        `shouldBe` Right [SimpleTypeImport "T" []]

    it "accepts an explicit constructor list" $
      importsOf "module M { import N(T(A)) }"
        `shouldBe` Right [SimpleTypeImport "T" ["A"]]

    it "accepts an empty import list" $
      importsOf "module M { import N() }" `shouldBe` Right []
