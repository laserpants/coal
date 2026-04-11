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
import Coal.Compiler.Build.Cache (writeBuildFile)
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Debug (writeDebugFile)
import Coal.Kernel.LLVM.IRConstruct (IRConstruct (..))
import Coal.Kernel.LLVM.IREncodable (irEncode)
import Coal.Kernel.LLVM.IRInterpreter.Monad (IRLine)
import Coal.Language.Module.Path
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT, protoOclearAssumptionsC, protoOclearNameStoreC, protoOgetCurrentBuildC, protoOinsertConstraintsC, protoOinsertNameC, protoOsetBitcodeC, protoOupdateSupplyC, setCurrentModuleC)
import Coal.ProtoCompiler.ProtoState
import Control.Exception (SomeException, try)
import Control.Monad.Catch (MonadMask)
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

passLLVMOutput :: (MonadIO m, MonadMask m) => Pass Metadata m [BuildEnvelope (Name, [IRConstruct [IRLine]])] [(Name, ByteString)]
passLLVMOutput = Pass{runPass = pass}

pass :: (MonadIO m, MonadMask m) => [BuildEnvelope (Name, [IRConstruct [IRLine]])] -> CompilerT Metadata (ProtoCompilerT m Metadata) [(Name, ByteString)]
pass ir = do
  config <- lift $ gets protoOcompilerConfig
  pb <- asks compilerProgressBar
  res <- generateLLOutput pb config ir
  case res of
    Left err ->
      throwError err
    Right results -> do
      lift $ forM_ results (uncurry protoOsetBitcodeC)
      modules_ <- lift $ gets protoOcompilerModules

      fresh <- lift $ gets protoOcompilerToBeRecompiled
      let freshModules = Environment.restrict (Set.toList fresh) modules_

      let buildDir = "./.build/"
      liftIO $ createDirectoryIfMissing True buildDir
      forM_ (Environment.toList freshModules) $
        uncurry (writeBuildFile buildDir)

      pure results

generateLLOutput :: (MonadIO m, MonadMask m) => Maybe ProgressBar -> CompilerConfig -> [BuildEnvelope (Name, [IRConstruct [IRLine]])] -> CompilerT Metadata (ProtoCompilerT m a) (Either CompilerFailureMode [(Name, ByteString)])
generateLLOutput pb CompilerConfig{..} mods = do
  withSystemTempDirectory "coal-build" $
    \tmpDir -> do
      llvmRes <- forM mods (irOutput pb CompilerConfig{..} tmpDir)
      let (lefts, rights) = partitionEithers llvmRes
      case lefts of
        [] -> do
          pure (Right rights)
        errs -> do
          liftIO $ putStrLn "llvm-as failed: "
          forM_ errs $ liftIO . print
          pure (Left CompilerError)

irOutput :: (MonadIO m) => Maybe ProgressBar -> CompilerConfig -> FilePath -> BuildEnvelope (Name, [IRConstruct [IRLine]]) -> CompilerT Metadata (ProtoCompilerT m a) (Either SomeException (Name, ByteString))
irOutput pb CompilerConfig{..} tmpDir = do
  \case
    BSource (name, code) -> do
      for_ pb $ liftIO . tick

      let file = tmpDir </> Text.unpack name <.> "ll"
          llCode = irEncode code

      liftIO $ Text.writeFile file llCode
      when configGenerateLLVMOutput $
        liftIO $
          writeDebugFile ("./.debug" </> Text.unpack name <.> "ll") llCode

      bs <- runLLVM tmpDir file
      pure (fmap (name,) bs)
    BCached ProtoBuild{..} ->
      pure (Right (principalPath protoObuildPath, fromJust protoObuildBitcode))

runLLVM :: (MonadIO m) => FilePath -> FilePath -> CompilerT Metadata (ProtoCompilerT m a) (Either SomeException ByteString)
runLLVM dir src =
  liftIO $
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
