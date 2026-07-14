{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Coal.Compiler.Pass.PhaseLowering.Linking (
  passLinking,
  compileBitcode,
) where

import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerFailureMode (CompilerError), CompilerT)
import Coal.Compiler.State (CompilerState (compilerConfig))
import Control.Exception (SomeException, try)
import Control.Monad.Except (MonadError (throwError), MonadIO (..), forM, forM_, unless, void)
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
passLinking = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => [(Name, ByteString)] -> CompilerT Metadata m ()
passImpl bcode = do
  config <- gets compilerConfig
  err <- liftIO (compileBitcode config bcode)
  for_ err throwError

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
          cFiles <- traverse canonicalizePath configCFiles
          forM_ cFiles $
            \file ->
              copyFile file (tmpDir </> takeBaseName file)
          gccRes <- runGCC tmpDir objFiles cFiles
          case gccRes of
            Left e -> do
              putStrLn ("gcc failed: " ++ show e)
              pure (Just CompilerError)
            Right _ -> do
              copyFile (tmpDir </> "dist") configExecutableName
              pure Nothing

runLLC :: FilePath -> Name -> ByteString -> IO (Either SomeException FilePath)
runLLC dir name bcode = do
  ByteString.writeFile file bcode
  try $ do
    execProcess $ (proc "llc" ["-filetype=obj", "-relocation-model=pic", file, "-o", target]){cwd = Just dir}
    pure target
 where
  file = dir </> Text.unpack name <.> "bc"
  target = takeBaseName file <.> "o"

runGCC :: FilePath -> [FilePath] -> [FilePath] -> IO (Either SomeException ())
runGCC dir objFiles cFiles = do
  isClang <- ("clang" `isInfixOf`) <$> readProcess "cc" ["--version"] ""
  let args =
        (if isClang then flags else "-no-pie" : flags) <> commonArgs
  try $ execProcess $ (proc "gcc" args){cwd = Just dir}
 where
  flags = ["-g", "-I.", "-fsanitize=address"]
  commonArgs =
    ["runtime.c"]
      <> cFiles
      <> objFiles
      <> ["-o", "dist"]
      <> ["-fsanitize=address"]
      <> ["-lgc", "-lgmp"]

execProcess :: CreateProcess -> IO ()
execProcess p = void $ readCreateProcessWithExitCode p ""

runtimeLib :: ByteString
runtimeLib = $(embedFile "runtime/dist/runtime-combined.c")