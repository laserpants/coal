{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Coal.Compiler.Pass.LoweringPhase.LLVMOutput (
  passLLVMOutput,
  generateLLOutput,
) where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (ModuleBuild (..))
import Coal.Compiler.Build.Cache (writeBuildFile)
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass (BuildUnit (..), Pass (..))
import Coal.Compiler.Stack
import Coal.DebugIO (writeDebugFile)
import Coal.Kernel.LLVM.IRConstruct (IRConstruct (..))
import Coal.Kernel.LLVM.IREncodable (irEncode)
import Coal.Kernel.LLVM.IRInterpreter.Monad (IRLine)
import Coal.Language.Module
import Control.Exception (SomeException, try)
import Control.Monad.Except
import Control.Monad.Reader (asks)
import Control.Monad.State (gets)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Extras (Name)
import System.Console.AsciiProgress (ProgressBar, tick)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import qualified System.Process.ByteString as ByteString

passLLVMOutput :: (MonadIO m) => Pass Metadata m [BuildUnit (Name, [IRConstruct [IRLine]])] [(Name, ByteString)]
passLLVMOutput = Pass{runPass = pass}

pass :: (MonadIO m) => [BuildUnit (Name, [IRConstruct [IRLine]])] -> CompilerT Metadata m [(Name, ByteString)]
pass ir = do
  config <- gets compilerConfig
  pb <- asks compilerProgressBar
  res <- liftIO $ generateLLOutput pb config ir
  case res of
    Left err ->
      throwError err
    Right results -> do
      forM_ results (uncurry setBitcodeC)
      modules_ <- gets compilerModules

      fresh <- gets compilerFreshModules
      let freshModules = Environment.restrict (Set.toList fresh) modules_

      let buildDir = "./.build/"
      liftIO $ createDirectoryIfMissing True buildDir
      forM_ (Environment.toList freshModules) $
        uncurry (writeBuildFile buildDir)

      pure results

generateLLOutput :: Maybe ProgressBar -> CompilerConfig -> [BuildUnit (Name, [IRConstruct [IRLine]])] -> IO (Either CompilerFailureMode [(Name, ByteString)])
generateLLOutput pb CompilerConfig{..} mods = do
  withSystemTempDirectory "coal-build" $
    \tmpDir -> do
      llvmRes <- forM mods (irOutput pb CompilerConfig{..} tmpDir)
      let (lefts, rights) = partitionEithers llvmRes
      case lefts of
        [] -> do
          pure (Right rights)
        errs -> do
          putStrLn "llvm-as failed: "
          forM_ errs print
          pure (Left CompilerError)

irOutput :: Maybe ProgressBar -> CompilerConfig -> FilePath -> BuildUnit (Name, [IRConstruct [IRLine]]) -> IO (Either SomeException (Name, ByteString))
irOutput pb CompilerConfig{..} tmpDir = do
  \case
    BSource (name, code) -> do
      for_ pb tick

      let file = tmpDir </> Text.unpack name <.> "ll"
          llCode = irEncode code

      Text.writeFile file llCode
      when configGenerateLLVMOutput $
        writeDebugFile ("./.debug" </> Text.unpack name <.> "ll") llCode

      bs <- runLLVM tmpDir file
      pure (fmap (name,) bs)
    BCached ModuleBuild{..} ->
      pure (Right (principalPath moduleBuildPath, fromJust moduleBitcode))

runLLVM :: FilePath -> FilePath -> IO (Either SomeException ByteString)
runLLVM dir src =
  try $ do
    (exit, out, err) <- ByteString.readCreateProcessWithExitCode process ""
    case exit of
      ExitSuccess ->
        pure out
      ExitFailure _ ->
        error $
          "llvm-as failed:\n"
            ++ (if ByteString.null err then "<no stderr>" else show err)
 where
  process =
    (proc "llvm-as" [src, "-o", "-"])
      { cwd = Just dir
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
