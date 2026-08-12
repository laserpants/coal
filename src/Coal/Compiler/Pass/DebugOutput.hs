{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.DebugOutput (
  generateDebugArtifacts,
  generateBuildInfo,
  writeDotFile,
) where

import Coal.Compiler.Build (Build (..))
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, getCurrentBuildC)
import Coal.Compiler.State (compilerConfig)
import Coal.Debug (writeDebugFile)
import Coal.Graphviz.Dot (Dot (..), generateDotSyntax)
import Coal.Language
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.Pretty (CoalPretty (..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
import Extras (forM_)
import System.FilePath ((<.>), (</>))
import Text.Pretty.Simple (pShowNoColor)

generateDebugArtifacts :: (MonadIO m, HasKind (Type Parameter k), CoalPretty k, Dot t, Dot k, Show k) => Text -> Pass a m (Module a k t) (Module a k t)
generateDebugArtifacts label = Pass{runPass = pass}
 where
  pass m = do
    CompilerConfig{..} <- gets compilerConfig
    when configGenerateDebugArtifacts $
      liftIO $
        writeDotFiles label m
    return m

generateBuildInfo :: (MonadIO m, Show a) => Text -> Pass a m (Module a k t) (Module a k t)
generateBuildInfo label = Pass{runPass = pass}
 where
  pass m = do
    CompilerConfig{..} <- gets compilerConfig
    when configGenerateDebugArtifacts (writeBuildInfo label)
    return m

{-# INLINE writeDotFile #-}
writeDotFile :: (Dot a) => Text -> a -> IO ()
writeDotFile fname a = writeDebugFile ("./.debug" </> Text.unpack fname <.> "gv") (generateDotSyntax a)

writeDotFiles :: (HasKind (Type Parameter k), CoalPretty k, Dot t, Dot k, Show k) => Text -> Module a k t -> IO ()
writeDotFiles ns m@(Module (Path path) _ defs) = do
  writeDotFile prefix m
  forM_ defs $
    \case
      def@(DFunction _ name _) ->
        writeDotFile (prefixedName name) def
      def@(DLet _ name _) ->
        writeDotFile (prefixedName name) def
      def@(DFold _ name _) ->
        writeDotFile (prefixedName name) def
      _ ->
        pure ()
 where
  prefix = ns <> "__" <> Text.intercalate "_" path
  prefixedName n = prefix <> "_" <> n

writeBuildInfo :: (MonadIO m, Show a) => Text -> CompilerT a m ()
writeBuildInfo label = do
  Build{..} <- getCurrentBuildC
  let path = principalPath buildPath
  liftIO $ do
    writeDebugFile (rootPath <> "_build_" <> Text.unpack path) (toStrict $ pShowNoColor $ Build{..})
    writeDebugFile (rootPath <> "_names_" <> Text.unpack path) (toStrict $ pShowNoColor buildNames)
 where
  rootPath = "./.debug" </> Text.unpack label
