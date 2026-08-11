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
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.FileEmbed (embedFile)
import Data.Foldable (for_)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import Extras (Name, forM)
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeBaseName, takeDirectory, (<.>), (</>))
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
          -- Copy each C source into the temp dir, preserving its path relative
          -- to the project root so that same-named files from different
          -- packages (e.g. two libraries each shipping an "event_source.c")
          -- do not clobber each other. The copied paths are what gcc compiles.
          cFiles <- traverse canonicalizePath configCFiles
          projectRoot <- getCurrentDirectory
          copiedCFiles <- forM cFiles $
            \file -> do
              let rel = makeRelative projectRoot file
                  target = tmpDir </> rel
              createDirectoryIfMissing True (takeDirectory target)
              copyFile file target
              pure target
          gccRes <- runGCC CompilerConfig{..} tmpDir objFiles copiedCFiles
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

runGCC :: CompilerConfig -> FilePath -> [FilePath] -> [FilePath] -> IO (Either SomeException ())
runGCC CompilerConfig{..} dir objFiles cFiles = do
  isClang <- ("clang" `isInfixOf`) <$> readProcess "cc" ["--version"] ""
  let args =
        (if isClang then flags else "-no-pie" : flags) <> commonArgs
  try $ execProcess $ (proc "gcc" args){cwd = Just dir}
 where
  sanitizeFlags = if configSanitize then ["-fsanitize=address"] else []
  flags = ["-g", "-I."] <> sanitizeFlags
  commonArgs =
    ["runtime.c"]
      <> cFiles
      <> objFiles
      <> ["-o", "dist"]
      <> sanitizeFlags
      <> ["-lgc", "-lgmp"]

execProcess :: CreateProcess -> IO ()
execProcess p = do
  (exitCode, _, stderr) <- readCreateProcessWithExitCode p ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code -> error $ "Process failed with exit code " ++ show code ++ ":\n" ++ stderr

runtimeLib :: ByteString
runtimeLib = $(embedFile "runtime/dist/runtime-combined.c")
