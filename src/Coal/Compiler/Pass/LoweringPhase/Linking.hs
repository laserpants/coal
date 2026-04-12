{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Coal.Compiler.Pass.LoweringPhase.Linking (passLinking, compileBitcode) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State (CompilerState (compilerConfig))
import Control.Exception (SomeException, try)
import Control.Monad.Except
import Control.Monad.State (gets)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.FileEmbed (embedFile)
import Data.Foldable (for_)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import Extras (Name)
import System.Directory (canonicalizePath, copyFile)
import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process

passLinking :: (MonadIO m) => Pass Metadata m [(Name, ByteString)] ()
passLinking = Pass{runPass = pass}

pass :: (MonadIO m) => [(Name, ByteString)] -> CompilerT Metadata m ()
pass bcode = do
  config <- gets compilerConfig
  r <- liftIO (compileBitcode config bcode)
  for_ r throwError

compileBitcode :: CompilerConfig -> [(Name, ByteString)] -> IO (Maybe CompilerFailureMode)
compileBitcode CompilerConfig{..} files =
  withSystemTempDirectory "coal-build" $
    \tmpDir -> do
      res <- forM files $ uncurry (runLLC tmpDir)
      case sequence res of
        Left err -> do
          print err
          pure (Just CompilerError)
        Right objFiles -> do
          ByteString.writeFile (tmpDir </> "runtime.c") runtimeLib
          ByteString.writeFile (tmpDir </> "hashmap.h") hashmapLib

          cFiles <- traverse canonicalizePath configCFiles
          forM_ cFiles $
            \file -> do
              copyFile file (tmpDir </> takeBaseName file)

          gccRes <- runGCC tmpDir objFiles cFiles
          case gccRes of
            Left e -> do
              putStrLn ("gcc failed: " ++ show e)
              pure (Just CompilerError)
            Right _ -> do
              copyFile (tmpDir </> "dist") configExecutableName
              unless configSilent $ do
                putStrLn ("Executable written to: " <> configExecutableName)
              pure Nothing

runLLC :: FilePath -> Name -> ByteString -> IO (Either SomeException FilePath)
runLLC dir name bcode = do
  ByteString.writeFile file bcode
  try $ do
    execProcess process
    pure target
 where
  file = dir </> Text.unpack name <.> "bc"
  process = (proc "llc" ["-filetype=obj", "-relocation-model=pic", file, "-o", target]){cwd = Just dir}
  target = takeBaseName file <.> "o"

runGCC :: FilePath -> [FilePath] -> [FilePath] -> IO (Either SomeException ())
runGCC dir objFiles cFiles = do
  isClang <- ("clang" `isInfixOf`) <$> readProcess "cc" ["--version"] ""
  let
    args = (if isClang then flags else "-no-pie" : flags) <> commonArgs
    process = (proc "gcc" args){cwd = Just dir}
  try $ execProcess process
 where
  flags = ["-g", "-I."]
  commonArgs =
    ["runtime.c"]
      <> cFiles
      <> objFiles
      <> ["-o", "dist"]
      <> ["-lgc", "-lgmp"]

execProcess :: CreateProcess -> IO ()
execProcess p = do
  (_, _, _, handle) <- createProcess p
  void $ waitForProcess handle

runtimeLib :: ByteString
runtimeLib = $(embedFile "runtime/lib.c")

hashmapLib :: ByteString
hashmapLib = $(embedFile "runtime/hashmap.h")
