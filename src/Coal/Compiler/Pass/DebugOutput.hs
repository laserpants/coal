{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.DebugOutput where -- (generateDebugArtifacts) where

--        Build{..} <- getCurrentBuildC
--        liftIO $ Text.writeFile ("tmp/aliases_build_" <> Text.unpack (principalPath modulePath)) (toStrict $ pShowNoColor $ Build{..})
--        liftIO $ Text.writeFile ("tmp/aliases_names_" <> Text.unpack (principalPath modulePath)) (toStrict $ pShowNoColor $ buildNames)

import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Compiler.State (compilerConfig)
import Coal.Debug (writeDebugFile)
import Coal.Graphviz.Dot (Dot (..), generateDotSyntax)
import Coal.Language
import Coal.Language.Module.Path (Path (..))
import Coal.Pretty (CoalPretty (..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (forM_)
import System.FilePath ((<.>), (</>))

generateDebugArtifacts :: (MonadIO m, HasKind (Type Parameter k), CoalPretty k, Dot t, Dot k, Show k) => Text -> Pass a m (Module a k t) (Module a k t)
generateDebugArtifacts ll = Pass{runPass = pass ll}

pass :: (MonadIO m, HasKind (Type Parameter k), CoalPretty k, Dot t, Dot k, Show k) => Text -> Module a k t -> CompilerT a m (Module a k t)
pass label m = do
  CompilerConfig{..} <- gets compilerConfig
  when configGenerateDotFiles $
    liftIO $
      writeDotFiles label m
  pure m

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
