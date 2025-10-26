{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.DebugOutput (generateDebugArtifacts) where

import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Graphviz.Dot (writeDotFile)
import Coal.Language.Module
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (forM_)
import Prettyprinter (Pretty (..))

generateDebugArtifacts :: (MonadIO m, Pretty t, Show t) => Text -> Pass a m (Module a k t) (Module a k t)
generateDebugArtifacts ll =
  Pass
    { passName = "debug<" <> ll <> ">"
    , runPass = pass ll
    }

pass :: (MonadIO m, Pretty t, Show t) => Text -> Module a k t -> CompilerT a m (Module a k t)
pass label m = do
  CompilerConfig{..} <- gets compilerConfig
  when configGenerateDotFiles $
    liftIO $
      writeDotFiles label m
  pure m

writeDotFiles :: (Pretty t, Show t) => Text -> Module a k t -> IO ()
writeDotFiles ns m@(Module (Path path) _ defs) = do
  writeDotFile prefix m
  forM_ defs $
    \case
      def@DFunction{} ->
        writeDotFile (prefixed $ definitionName def) def
      def@DConstant{} ->
        writeDotFile (prefixed $ definitionName def) def
      _ ->
        pure ()
 where
  prefix = ns <> "__" <> Text.intercalate "_" path
  prefixed n = prefix <> "_" <> n
