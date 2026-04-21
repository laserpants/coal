{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.DebugOutput where -- (generateDebugArtifacts) where

--        Build{..} <- getCurrentBuildC
--        liftIO $ Text.writeFile ("tmp/aliases_build_" <> Text.unpack (principalPath modulePath)) (toStrict $ pShowNoColor $ Build{..})
--        liftIO $ Text.writeFile ("tmp/aliases_names_" <> Text.unpack (principalPath modulePath)) (toStrict $ pShowNoColor $ buildNames)

import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State (compilerConfig)
import Coal.Language
import Coal.Language.Module.Path (Path (..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (forM_)
import Prettyprinter (Pretty (..))

generateDebugArtifacts :: (MonadIO m, Pretty t, Show t) => Text -> Pass a m (Module a k t) (Module a k t)
generateDebugArtifacts ll = Pass{runPass = pass ll}

pass :: (MonadIO m, Pretty t, Show t) => Text -> Module a k t -> CompilerT a m (Module a k t)
pass label m = do
  CompilerConfig{..} <- gets compilerConfig
  when configGenerateDotFiles $
    liftIO $
      writeDotFiles label m
  pure m

writeDotFiles :: (Pretty t, Show t) => Text -> Module a k t -> IO ()
writeDotFiles ns m@(Module (Path path) _ defs) = do
  undefined

--  writeDotFile prefix m
--  forM_ defs $
--    \case
--      def@DFunction{} ->
--        writeDotFile (prefixedName def) def
--      def@DLet{} ->
--        writeDotFile (prefixedName def) def
--      def@DFold{} ->
--        writeDotFile (prefixedName def) def
--      _ ->
--        pure ()
-- where
--  prefix = ns <> "__" <> Text.intercalate "_" path
--  prefixedName n = prefix <> "_" <> definitionName n
