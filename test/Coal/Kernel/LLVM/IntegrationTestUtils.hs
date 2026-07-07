{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Kernel.LLVM.IntegrationTestUtils
Description: Integration test utilities for end-to-end compilation

This module provides utilities for testing the complete Coal compilation pipeline,
from parsing through normalization to LLVM IR generation and linking.
-}
module Coal.Kernel.LLVM.IntegrationTestUtils (
  -- * Full Pipeline Tests
  testCompilePipeline,
  testCompilePipelineWithC,
  testCompile,

  -- * Test Runners
  runTest,
  runTestForFile,
) where

import Coal.Kernel.LLVM.TestUtils (evaluateFoo, injectBuiltins)
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Module.DependencyGraph (checkImportsSatisfied, topoSortModules)
import qualified Coal.Kernel.Parser.Module as Parser
import Coal.Kernel.Pipeline (evalPipeline, initialPipelineState)
import Coal.Kernel.Pipeline.Passes (pipeline)
import Coal.Kernel.Prettyprinter (renderModule)
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import LLVM.IR
import qualified LLVM.IRRenderer
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, (</>))
import System.Process (callProcess)
import Text.Megaparsec (errorBundlePretty, parse)

{- | Test the full compilation pipeline: parse → normalize → compile → link.

This test runs the complete pipeline including all normalization passes,
writes the normalized modules to @.debug/@, and links the result with the
runtime.
-}
testCompilePipeline :: [FilePath] -> IO ()
testCompilePipeline cornFiles = testCompilePipelineWithC cornFiles []

{- | Like 'testCompilePipeline', but also links additional C source files.

The extra C files are appended to the @clang@ invocation after the
runtime, so they can reference Coal-generated symbols or the runtime API.
-}
testCompilePipelineWithC :: [FilePath] -> [FilePath] -> IO ()
testCompilePipelineWithC cornFiles extraCFiles = do
  createDirectoryIfMissing True buildDir
  -- Phase 1: parse all modules and inject builtin constructors.
  allModules <- map injectBuiltins <$> traverse parseOne cornFiles
  -- Phase 2: run the full normalization pipeline on each module.
  allNormalized <- traverse runPipeline_ allModules
  -- Sort in dependency order (imports before importers); abort on cycles.
  let nameToFile = Map.fromList [(moduleName m, f) | (f, m) <- zip cornFiles allNormalized]
  sortedNormalized <- case topoSortModules allNormalized of
    Left names ->
      error ("Module dependency cycle: " <> Text.unpack (Text.intercalate ", " names))
    Right sorted -> do
      let missing = checkImportsSatisfied sorted
      unless (null missing) $
        error ("Unsatisfied module imports: " <> show missing)
      return sorted
  -- Phase 2b: write prettyprinted normalized modules to .debug/
  createDirectoryIfMissing True debugDir
  mapM_ writeNormalized sortedNormalized
  -- Phase 3: compile each normalized module with the full module list for context.
  let sortedPairs = [(nameToFile Map.! moduleName m, m) | m <- sortedNormalized]
  llFiles <- traverse (compileOne allNormalized) sortedPairs
  callProcess "clang" $
    ["-Wno-override-module", "-lgc", "-lgmp"]
      <> llFiles
      <> [mainLl, runtimeC]
      <> extraCFiles
      <> ["-o", buildDir </> "dist"]
 where
  buildDir = ".build"
  debugDir = ".debug"
  mainLl = ".tmp/main.ll"
  runtimeC = "runtime-next/dist/runtime-combined.c"
  writeNormalized m = do
    let outFile = debugDir </> Text.unpack (moduleName m) <> ".normalized.corn"
    Text.writeFile outFile (renderModule m)
  parseOne fname = do
    content <- Text.readFile fname
    case parse Parser.module_ "" content of
      Left err -> error (errorBundlePretty err)
      Right m -> return m
  runPipeline_ m =
    case evalPipeline initialPipelineState (pipeline m) of
      Left err -> error (show err)
      Right m' -> return m'
  compileOne allMods (fname, m) = do
    ir <- case evaluateFoo mempty emptyIRBuilderEnv allMods m of
      Left err -> error err
      Right r -> return r
    let outFile = buildDir </> takeBaseName fname <> ".ll"
    Text.writeFile outFile (LLVM.IRRenderer.renderModule ir)
    return outFile

{- | Test compilation without running the normalization pipeline.

This test parses and compiles modules directly without normalization,
useful for testing the code generator in isolation.
-}
testCompile :: [FilePath] -> IO ()
testCompile cornFiles = do
  createDirectoryIfMissing True buildDir
  -- Phase 1: parse all modules, inject builtin constructors, so that
  -- cross-module DData info is available.
  allModules <- map injectBuiltins <$> traverse parseOne cornFiles
  -- Sort in dependency order; abort on cycles or missing imports.
  let nameToFile = Map.fromList [(moduleName m, f) | (f, m) <- zip cornFiles allModules]
  sortedModules <- case topoSortModules allModules of
    Left names ->
      error ("Module dependency cycle: " <> Text.unpack (Text.intercalate ", " names))
    Right sorted -> do
      let missing = checkImportsSatisfied sorted
      unless (null missing) $
        error ("Unsatisfied module imports: " <> show missing)
      return sorted
  -- Phase 2: compile each module in dependency order.
  let sortedPairs = [(nameToFile Map.! moduleName m, m) | m <- sortedModules]
  llFiles <- traverse (compileOne sortedModules) sortedPairs
  callProcess "clang" $
    ["-Wno-override-module", "-lgc", "-lgmp"]
      <> llFiles
      <> [mainLl, runtimeC]
      <> ["-o", buildDir </> "dist"]
 where
  buildDir = ".build"
  mainLl = ".tmp/main.ll"
  runtimeC = "runtime-next/dist/runtime-combined.c"
  parseOne fname = do
    content <- Text.readFile fname
    case parse Parser.module_ "" content of
      Left err -> error (errorBundlePretty err)
      Right m -> return m
  compileOne allMods (fname, m) = do
    ir <- case evaluateFoo mempty emptyIRBuilderEnv allMods m of
      Left err -> error err
      Right r -> return r
    let outFile = buildDir </> takeBaseName fname <> ".ll"
    Text.writeFile outFile (LLVM.IRRenderer.renderModule ir)
    return outFile

-- | Run a test on a file, parsing and compiling it, then printing the result.
runTestForFile :: FilePath -> IO ()
runTestForFile fname = do
  content <- Text.readFile fname
  case parse Parser.module_ "" content of
    Left err ->
      error (errorBundlePretty err)
    Right m ->
      runTest m

-- | Run a test on a parsed module, compiling it and printing the LLVM IR.
runTest :: Module Type -> IO ()
runTest module_ =
  case evaluateFoo mempty emptyIRBuilderEnv [] module_ of
    Right r ->
      Text.putStrLn (LLVM.IRRenderer.renderModule r)
    Left err ->
      error (show err)
