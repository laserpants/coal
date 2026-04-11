{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.ImportsTopRule (passImportsTopRule) where

import Coal.AST.HasMetadata (HasMetadata (..))
import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Definition
import Coal.Language.Module (Module (..), ModuleExportList (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad.Except (MonadError (throwError), MonadIO, forM_, unless)

passImportsTopRule :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passImportsTopRule = Pass{runPass = pass}

pass :: (Monad m) => [BuildEnvelope (Module Metadata () ())] -> CompilerT Metadata m [BuildEnvelope (Module Metadata () ())]
pass ms = do
  (ps, errors) <- listenErrors $ traverse (traverse checkImports) ms
  unless (null errors) $
    throwError PreflightFailure
  pure ps

checkImports :: (Monad m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
checkImports m = do
  forM_ (filter isImport es) $
    \d ->
      tellErrors [MisplacedImportStatement (ErrorLocation name (getMetadata d))]
  pure mm
 where
  mm@(Module p _ defs) = m
  name = principalPath p
  ds = dropWhile isImport defs
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

-- checkImports :: (Monad m) => Module Metadata k () -> CompilerT Metadata m (Module Metadata k ())
-- checkImports m@(Module p _ defs) = do
--  forM_ (filter isImport es) $
--    \d ->
--      tellErrors [MisplacedImportStatement (ErrorLocation name (getMetadata d))]
--  pure m
-- where
--  name = principalPath p
--  ds = dropWhile isImport defs
--  es = dropWhile (not . isImport) ds
