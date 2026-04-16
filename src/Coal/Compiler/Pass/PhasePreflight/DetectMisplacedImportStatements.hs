{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PhasePreflight.DetectMisplacedImportStatements (
  passDetectMisplacedImportStatements,
) where

import Coal.AST.HasMetadata (HasMetadata (..))
import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (
  CompilerError (MisplacedImportStatement),
  CompilerFailureMode (PreflightFailure),
  CompilerT,
  ErrorLocation (ErrorLocation),
 )
import Coal.Language.Definition (Definition (DImport, DNamespaceImport))
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad.Except (MonadError (throwError), MonadIO, forM_, unless)

passDetectMisplacedImportStatements :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passDetectMisplacedImportStatements = Pass{runPass = passImpl}

passImpl :: (Monad m) => [BuildEnvelope (Module Metadata () ())] -> CompilerT Metadata m [BuildEnvelope (Module Metadata () ())]
passImpl ins = do
  (outs, errors) <- listenErrors $ traverse (traverse detectMisplacedImportStatements) ins
  unless (null errors) $
    throwError PreflightFailure
  pure outs

detectMisplacedImportStatements :: (Monad m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
detectMisplacedImportStatements Module{..} = do
  forM_ (filter isImport es) $
    \def ->
      tellErrors [MisplacedImportStatement (ErrorLocation (principalPath modulePath) (getMetadata def))]
  pure Module{..}
 where
  ds = dropWhile isImport moduleDefinitions
  es = dropWhile (not . isImport) ds

isImport :: Definition a k t -> Bool
isImport =
  \case
    DImport{} ->
      True
    DNamespaceImport{} ->
      True
    _ ->
      False
