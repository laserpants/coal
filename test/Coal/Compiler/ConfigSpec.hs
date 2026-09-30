{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Compiler.ConfigSpec
Description: Tests for the build-cache configuration hash.

'configHash' gates every cache hit: a cached build is only reused when both
its source hash and its config hash match. These tests pin which
'CompilerConfig' fields participate, so that a field which changes generated
code cannot be added without invalidating caches, and a purely diagnostic
field cannot be added in a way that needlessly thrashes them.
-}
module Coal.Compiler.ConfigSpec (configHashSpec) where

import Coal.Compiler.Config (CompilerConfig (..), configHash, defaultConfig)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

configHashSpec :: Spec
configHashSpec =
  describe "configHash" $ do
    describe "fields that affect generated code" $ do
      it "changes when the entry point changes" $
        configHash (defaultConfig{configEntryPoint = Just ("Main", "main")})
          `shouldNotBe` configHash defaultConfig

      it "changes when the entry point function changes" $
        configHash (defaultConfig{configEntryPoint = Just ("Main", "run")})
          `shouldNotBe` configHash (defaultConfig{configEntryPoint = Just ("Main", "main")})

      it "changes when sanitize is toggled" $
        configHash (defaultConfig{configSanitize = True})
          `shouldNotBe` configHash defaultConfig

      it "changes when a C source is added" $
        configHash (defaultConfig{configCFiles = ["/tmp/proj/counter.c"]})
          `shouldNotBe` configHash defaultConfig

      it "changes when package namespaces change" $
        configHash (defaultConfig{configPackageNamespaces = [("/tmp/pkg", "Pkg", ["MicroTest"])]})
          `shouldNotBe` configHash defaultConfig

    describe "configCFiles" $ do
      it "ignores the directory, hashing only the basename" $
        -- The linker canonicalizes these to absolute paths, so hashing the
        -- full path would make the cache machine-specific.
        configHash (defaultConfig{configCFiles = ["/a/b/counter.c"]})
          `shouldBe` configHash (defaultConfig{configCFiles = ["/x/y/counter.c"]})

      it "distinguishes different basenames" $
        configHash (defaultConfig{configCFiles = ["/a/counter.c"]})
          `shouldNotBe` configHash (defaultConfig{configCFiles = ["/a/other.c"]})

    describe "fields that cannot change generated code" $ do
      -- These are pinned deliberately: hashing them would force a full
      -- rebuild whenever a diagnostic flag is toggled.
      it "ignores the executable name" $
        configHash (defaultConfig{configExecutableName = "other"})
          `shouldBe` configHash defaultConfig

      it "ignores configSilent" $
        configHash (defaultConfig{configSilent = True})
          `shouldBe` configHash defaultConfig

      it "ignores configShowTiming" $
        configHash (defaultConfig{configShowTiming = True})
          `shouldBe` configHash defaultConfig

      it "ignores configGenerateDebugArtifacts" $
        configHash (defaultConfig{configGenerateDebugArtifacts = True})
          `shouldBe` configHash defaultConfig

      it "ignores configGenerateLLVMOutput" $
        configHash (defaultConfig{configGenerateLLVMOutput = True})
          `shouldBe` configHash defaultConfig

      it "ignores configNoCache" $
        configHash (defaultConfig{configNoCache = True})
          `shouldBe` configHash defaultConfig

      it "ignores configSourcePaths" $
        -- Absolute paths would break sharing between checkouts, and the
        -- resolved source text is already covered by the per-module hash.
        configHash (defaultConfig{configSourcePaths = ["/some/checkout/src"]})
          `shouldBe` configHash defaultConfig

    it "is stable across repeated evaluation" $
      configHash defaultConfig `shouldBe` configHash defaultConfig
