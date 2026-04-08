{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.ParsingPhase.Parsing (passParsing, fromSource) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Cache (cachedBuild)
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Config
import Coal.Compiler.Embedded (embedded)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Path.Resolve (resolveModule)
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module.Path (principalPath)
import Coal.Parser (ParserError, parseModule)
import Coal.Parser.Core (spaces)
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import qualified Data.ByteString as B
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as Text
import Extras (Name, forM, forM_)
import Text.Megaparsec (eof, runParser)

passParsing :: (MonadIO m) => Pass Metadata m [FilePath] [BuildUnit (ProtoModule Metadata () ())]
passParsing = Pass{runPass = pass}

pass :: (MonadIO m) => [FilePath] -> CompilerT Metadata m [BuildUnit (ProtoModule Metadata () ())]
pass files = do
  embeddedFiles <- traverse parseEmbedded embedded
  case partitionEithers embeddedFiles of
    ([], embeddedBundle) -> do
      results <- traverse parseFile files
      case partitionEithers results of
        ([], bundle) ->
          pure (embeddedBundle <> bundle)
        (es, _) -> do
          forM_ es (tellErrors . return)
          throwError ParserFailure
    (es, _) -> do
      forM es $
        \(p, e) ->
          error ("Error in embedded module '" <> Text.unpack p <> "': " <> show e)

parseEmbedded :: (MonadIO m) => (Text, B.ByteString) -> CompilerT Metadata m (Either (Text, ParserError) (BuildUnit (ProtoModule Metadata () ())))
parseEmbedded (p, src) = do
  CompilerConfig{..} <- gets compilerConfig
  case runParser (spaces *> parseModule <* eof) "" encodedSrc of
    Left err ->
      pure $ Left (p, err)
    Right module_ -> do
      let name = principalPath (protoOmodulePath module_)
      -- Check cached build files
      cached <- cachedBuild name encodedSrc
      -- TODO:
      --      setVerbatimSourceForC module_ encodedSrc
      case cached of
        Just mb | not configNoCache -> do
          insertModuleC name mb
          pure $ Right (BCached mb)
        _ -> do
          insertFreshModule name
          pure $ Right (BSource module_)
 where
  encodedSrc :: Text
  encodedSrc = E.decodeUtf8 src

fromSource :: (MonadIO m) => Name -> FilePath -> Text -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildUnit (ProtoModule Metadata () ())))
fromSource name file src = do
  insertFreshModule name
  case runParser (spaces *> parseModule <* eof) "" src of
    Left err ->
      pure $ Left (ParserError file err)
    Right module_@(ProtoModule path _ _) -> do
      if principalPath path == name
        then do
          pure $ Right (BSource module_)
        else pure $ Left (BadModuleName file (principalPath path))

parseFile :: (MonadIO m) => FilePath -> CompilerT Metadata m (Either (CompilerError Metadata) (BuildUnit (ProtoModule Metadata () ())))
parseFile file = do
  CompilerConfig{..} <- gets compilerConfig
  res <- liftIO $ resolveModule configSourcePaths file
  case res of
    Right (fp, _, name) -> do
      src <- liftIO (Text.readFile fp)
      -- Check cached build files
      cached <- cachedBuild name src
      setVerbatimSourceC name src
      case cached of
        Just mb | not configNoCache -> do
          insertModuleC name mb
          pure $ Right (BCached mb)
        _ ->
          fromSource name file src
    Left err -> do
      pure $ Left (BadFilename file err)
