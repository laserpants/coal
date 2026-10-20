{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the parsing pass's package-namespace handling.

These pin the collision policy: a module defined by the project always shadows a
same-named package module, a short name provided by more than one package is
reported as ambiguous, and a locally-declared module is never resolved from a
dependency's source tree.
-}
module Coal.Compiler.Pass.PhaseParsing.ParsingSpec (parsingSpec) where

import Coal.Compiler.Pass.PhaseParsing.Parsing (
  ambiguousPackageModules,
  buildPackageNsMap,
  localAwareNsMap,
  packageModuleProviders,
 )
import Coal.Compiler.Path.Resolve (resolveModule)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

{- | Two packages that both export @Spec@, plus distinct @Widget@/@Gadget@
modules. The order of the list is significant: it is the deterministic provider
order the compiler relies on.
-}
namespaces :: [(FilePath, Text, [Text])]
namespaces =
  [ ("/pkgs/alpha", "Alpha", ["Spec", "Widget"])
  , ("/pkgs/beta", "Beta", ["Spec", "Gadget"])
  ]

parsingSpec :: Spec
parsingSpec = do
  describe "packageModuleProviders" $ do
    it "maps a short name to every namespaced provider" $
      Map.lookup "Spec" (packageModuleProviders namespaces)
        `shouldBe` Just ["Alpha.Spec", "Beta.Spec"]

    it "maps an unambiguous short name to its single provider" $
      Map.lookup "Widget" (packageModuleProviders namespaces)
        `shouldBe` Just ["Alpha.Widget"]

  describe "ambiguousPackageModules" $ do
    it "reports short names provided by more than one package" $
      Map.keys (ambiguousPackageModules namespaces)
        `shouldBe` ["Spec"]

    it "does not report unambiguous short names" $
      Map.member "Widget" (ambiguousPackageModules namespaces)
        `shouldBe` False

  describe "buildPackageNsMap" $ do
    it "maps an unambiguous short name to its namespaced name" $
      Map.lookup "Widget" (buildPackageNsMap namespaces)
        `shouldBe` Just "Alpha.Widget"

    it "picks the first provider for an ambiguous name deterministically" $
      Map.lookup "Spec" (buildPackageNsMap namespaces)
        `shouldBe` Just "Alpha.Spec"

  describe "localAwareNsMap" $ do
    it "drops short names that are defined locally" $
      Map.member "Spec" (localAwareNsMap ["Spec"] (buildPackageNsMap namespaces))
        `shouldBe` False

    it "keeps short names that are not defined locally" $
      Map.lookup "Widget" (localAwareNsMap ["Spec"] (buildPackageNsMap namespaces))
        `shouldBe` Just "Alpha.Widget"

  describe "resolveModule" $ do
    it "does not search package roots for a local module name" $
      withSystemTempDirectory "coal-resolve" $ \root -> do
        let localDir = root </> "local"
            pkgDir = root </> "pkg"
        createDirectoryIfMissing True localDir
        createDirectoryIfMissing True pkgDir
        writeFile (pkgDir </> "MyModule.coal") "module MyModule {}"
        -- Only the project's own root is searched, so the package's copy must
        -- not be found even though it is a known root.
        res <- resolveModule [localDir] [localDir, pkgDir] "MyModule.coal"
        res `shouldSatisfy` isLeft

    it "finds a local module and classifies it under the local root" $
      withSystemTempDirectory "coal-resolve" $ \root -> do
        let localDir = root </> "local"
            pkgDir = root </> "pkg"
        createDirectoryIfMissing True localDir
        createDirectoryIfMissing True pkgDir
        writeFile (localDir </> "MyModule.coal") "module MyModule {}"
        canonLocal <- canonicalizePath localDir
        res <- resolveModule [localDir] [localDir, pkgDir] "MyModule.coal"
        case res of
          Right (_, bestRoot, derivedName) -> do
            bestRoot `shouldBe` canonLocal
            derivedName `shouldBe` "MyModule"
          Left err -> expectationFailure err
