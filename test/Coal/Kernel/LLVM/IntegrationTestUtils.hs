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
) where

import Coal.Kernel.LLVM.TestUtils (evaluateModule, injectBuiltins)
import Coal.Kernel.Language.Module (Module (..))
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
  allModules <- fmap injectBuiltins <$> traverse parseOne cornFiles
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
      <> [runtimeC]
      <> extraCFiles
      <> ["-o", buildDir </> "dist"]
 where
  buildDir = ".build"
  debugDir = ".debug"
  runtimeC = "runtime/dist/runtime-combined.c"
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
    ir <- case evaluateModule mempty emptyIRBuilderEnv allMods m of
      Left err -> error err
      Right r -> return r
    let outFile = buildDir </> takeBaseName fname <> ".ll"
    Text.writeFile outFile (LLVM.IRRenderer.renderModule ir)
    return outFile
