{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Coal.Compiler.Pass.LoweringPhase.LLVMOutput (passLLVMOutput) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.DebugIO (writeDebugFile)
import Coal.Kernel.LLVM.IRConstruct (IRConstruct (..))
import Coal.Kernel.LLVM.IREncodable (irEncode)
import Coal.Kernel.LLVM.IRInterpreter.Monad
import Control.Exception (SomeException, try)
import Control.Monad.Except
import Control.Monad.State (gets)
import qualified Data.ByteString as B
import Data.Either (lefts, rights)
import Data.FileEmbed (embedFile)
import Data.Foldable (for_)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Extras (Name)
import System.Directory (copyFile)
import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (createProcess, cwd, proc, waitForProcess)

passLLVMOutput :: (MonadIO m) => Pass Metadata m [(Name, [IRConstruct [IRLine]])] ()
passLLVMOutput =
  Pass
    { passName = "LLVMOutput"
    , runPass = pass
    }

pass :: (MonadIO m) => [(Name, [IRConstruct [IRLine]])] -> CompilerT Metadata m ()
pass ir = do
  config <- gets compilerConfig
  r <- liftIO $ generateLLOutput config ir
  for_ r throwError

runtimeLib :: B.ByteString
runtimeLib = $(embedFile "runtime/lib.c")

hashmapLib :: B.ByteString
hashmapLib = $(embedFile "runtime/hashmap.h")

generateLLOutput :: CompilerConfig -> [(Name, [IRConstruct [IRLine]])] -> IO (Maybe CompilerFailureMode)
generateLLOutput CompilerConfig{..} mods = do
  withSystemTempDirectory "coal-build" $
    \tmpDir -> do
      files <-
        forM mods $
          \(name, code) -> do
            let file = tmpDir </> Text.unpack name <.> "ll"
                llCode = irEncode code
            Text.writeFile file llCode
            when configGenerateLLVMOutput $
              writeDebugFile ("./.debug" </> Text.unpack name <.> "ll") llCode
            pure file

      B.writeFile (tmpDir </> "runtime.c") runtimeLib
      B.writeFile (tmpDir </> "hashmap.h") hashmapLib

      llcRes <- traverse (runLLC tmpDir) files

      case lefts llcRes of
        [] -> do
          gccRes <- runGCC tmpDir (rights llcRes)
          case gccRes of
            Left e -> do
              putStrLn ("gcc failed: " ++ show e)
              pure (Just CompilerError)
            Right _ -> do
              copyFile (tmpDir </> "dist") configExecutableName
              pure Nothing
        errs -> do
          putStrLn "llc failed: "
          forM_ errs print
          pure (Just CompilerError)

runGCC :: FilePath -> [FilePath] -> IO (Either SomeException ())
runGCC tmpDir objFiles =
  try $ do
    (_, _, _, ph) <- createProcess procSpec
    _ <- waitForProcess ph
    pure ()
 where
  procSpec = (proc "gcc" args){cwd = Just tmpDir}
  args =
    ["-g", "-I."]
      <> ["runtime.c"]
      <> objFiles
      <> ["-o", "dist"]
      <> ["-lgc", "-lgmp"]

runLLC :: FilePath -> FilePath -> IO (Either SomeException FilePath)
runLLC tmpDir llFile =
  try $ do
    (_, _, _, ph) <- createProcess procSpec
    _ <- waitForProcess ph
    pure target
 where
  procSpec = (proc "llc" ["-filetype=obj", llFile, "-o", target]){cwd = Just tmpDir}
  target = takeBaseName llFile <.> "o"
