{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.ImportsTopRule (passImportsTopRule) where

import Coal.AST.HasMetadata (HasMetadata (..))
import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Definition (isImport)
import Coal.Language.Module.Path (principalPath)
import Control.Monad.Except (MonadError (throwError), MonadIO, forM_, unless)

passImportsTopRule :: (MonadIO m) => Pass Metadata m [BuildUnit (Module Metadata k ())] [BuildUnit (Module Metadata k ())]
passImportsTopRule = Pass{runPass = pass}

pass :: (Monad m) => [BuildUnit (Module Metadata k ())] -> CompilerT Metadata m [BuildUnit (Module Metadata k ())]
pass ms = do
  (ps, errors) <- listenErrors $ traverse (traverse checkImports) ms
  unless (null errors) $
    throwError PreflightFailure
  pure ps

checkImports :: (Monad m) => Module Metadata k () -> CompilerT Metadata m (Module Metadata k ())
checkImports m@(Module p _ defs) = do
  forM_ (filter isImport es) $
    \d ->
      tellErrors [MisplacedImportStatement (ErrorLocation name (getMetadata d))]
  pure m
 where
  name = principalPath p
  ds = dropWhile isImport defs
  es = dropWhile (not . isImport) ds
