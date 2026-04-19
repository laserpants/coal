{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PhaseParsing.Parsing (passParsing, fromSource) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Cache (cachedBuild)
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Builtin.Modules (builtinModules)
import Coal.Compiler.Config
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Path.Resolve (resolveModule)
import Coal.Compiler.Stack
import Coal.Compiler.State (CompilerState (compilerConfig))
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Coal.Parser (ParserError, parseSourceFile)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import qualified Data.ByteString as B
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as Text
import Extras (Name, forM_)
import Text.Megaparsec (runParser)

passParsing :: (MonadIO m) => Pass Metadata m [FilePath] [BuildEnvelope (Module Metadata () ())]
passParsing = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => [FilePath] -> CompilerT Metadata m [BuildEnvelope (Module Metadata () ())]
passImpl files = do
  builtinModulesFiles <- traverse parseEmbedded builtinModules
  builtinModulesBundle <- handleParseResults builtinModulesFiles $ \(p, e) ->
    error ("Error in builtinModules module '" <> Text.unpack p <> "': " <> show e)
  results <- traverse parseFile files
  bundle <- handleParseResults results (tellErrors . return)
  pure (builtinModulesBundle <> bundle)

-- | Helper to handle parsing results: report errors or return bundles
handleParseResults :: (MonadIO m) => [Either e a] -> (e -> CompilerT Metadata m ()) -> CompilerT Metadata m [a]
handleParseResults results reportError = do
  case partitionEithers results of
    ([], bundles) ->
      pure bundles
    (errors, _) -> do
      forM_ errors reportError
      throwError ParserFailure

-- | Check cache and handle source registration for a given module name and source
checkCacheAndRegister :: (MonadIO m) => Name -> Text -> Module Metadata () () -> CompilerT Metadata m (BuildEnvelope (Module Metadata () ()))
checkCacheAndRegister name src m = do
  CompilerConfig{configNoCache} <- gets compilerConfig
  cached <- cachedBuild name src
  setBuildSourceC name src
  case cached of
    Just build | not configNoCache -> do
      insertBuildC build
      pure (BCached build)
    _ -> do
      setTouched name
      pure (BSource m)

parseEmbedded :: (MonadIO m) => (Text, B.ByteString) -> CompilerT Metadata m (Either (Text, ParserError) (BuildEnvelope (Module Metadata () ())))
parseEmbedded (p, src) = do
  let encodedSrc = E.decodeUtf8 src
  case runParser parseSourceFile "" encodedSrc of
    Left err ->
      pure $ Left (p, err)
    Right m -> do
      let name = principalPath (modulePath m)
      Right <$> checkCacheAndRegister name encodedSrc m

fromSource :: (MonadIO m) => Name -> FilePath -> Text -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildEnvelope (Module Metadata () ())))
fromSource name file src = do
  case runParser parseSourceFile "" src of
    Left err ->
      pure $ Left (ParserError file err)
    Right m@(Module path _ _) ->
      if principalPath path == name
        then Right <$> checkCacheAndRegister name src m
        else pure $ Left (BadModuleName file (principalPath path))

parseFile :: (MonadIO m) => FilePath -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildEnvelope (Module Metadata () ())))
parseFile file = do
  CompilerConfig{..} <- gets compilerConfig
  res <- liftIO $ resolveModule configSourcePaths file
  case res of
    Right (fp, _, name) -> do
      src <- liftIO (Text.readFile fp)
      fromSource name file src
    Left err ->
      pure $ Left (BadFilename file err)
