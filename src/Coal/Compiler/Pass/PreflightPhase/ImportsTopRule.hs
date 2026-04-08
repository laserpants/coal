{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Compiler.Pass.PreflightPhase.ImportsTopRule (passImportsTopRule) where

import Coal.ProtoLanguage.ProtoDefinition
import Coal.AST.HasMetadata (HasMetadata (..))
import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module (Module (..), toProtoModule)
import Coal.Language.Module.Path (principalPath)
import Control.Monad.Except (MonadError (throwError), MonadIO, forM_, unless)
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))

passImportsTopRule :: (MonadIO m) => Pass Metadata m [BuildUnit (ProtoModule Metadata () ())] [BuildUnit (ProtoModule Metadata () ())]
passImportsTopRule = Pass{runPass = pass}

pass :: (Monad m) => [BuildUnit (ProtoModule Metadata () ())] -> CompilerT Metadata m [BuildUnit (ProtoModule Metadata () ())]
pass ms = do
  (ps, errors) <- listenErrors $ traverse (traverse checkImports) ms
  unless (null errors) $
    throwError PreflightFailure
  pure ps

checkImports :: (Monad m) => ProtoModule Metadata () () -> CompilerT Metadata m (ProtoModule Metadata () ())
checkImports m = do
  forM_ (filter isImport es) $
    \d ->
      tellErrors [MisplacedImportStatement (ErrorLocation name (getMetadata d))]
  pure mm
  where
    mm@(ProtoModule p _ defs) = m
    name = principalPath p
    ds = dropWhile isImport defs
    es = dropWhile (not . isImport) ds

isImport :: ProtoDefinition a k t -> Bool
isImport =
  \case
    ProtoDImport{} ->
      True
    ProtoDNamespaceImport{} ->
      True
    _ ->
      False

--checkImports :: (Monad m) => Module Metadata k () -> CompilerT Metadata m (Module Metadata k ())
--checkImports m@(Module p _ defs) = do
--  forM_ (filter isImport es) $
--    \d ->
--      tellErrors [MisplacedImportStatement (ErrorLocation name (getMetadata d))]
--  pure m
-- where
--  name = principalPath p
--  ds = dropWhile isImport defs
--  es = dropWhile (not . isImport) ds
