{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.ImportsTopRule (passImportsTopRule) where

import Coal.AST.Metadata (HasMetadata (..), Metadata (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module (Definition, Module, modulePathName, overModuleDefinitionsM)
import Coal.Language.Module.Definition (isImport)
import Control.Monad.Except (MonadError (throwError), MonadIO, forM_, unless)
import Extras (Name)

passImportsTopRule :: (MonadIO m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
passImportsTopRule = Pass{runPass = pass}

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT Metadata m [Module Metadata Kind ()]
pass ms = do
  (ps, errors) <- listenErrors $ traverse (\m -> overModuleDefinitionsM (checkImports (modulePathName m)) m) ms
  unless (null errors) $
    throwError PreflightFailure
  pure ps

checkImports :: (Monad m) => Name -> [Definition Metadata k ()] -> CompilerT Metadata m [Definition Metadata k ()]
checkImports name defs = do
  forM_ (filter isImport es) $
    \d ->
      tellErrors [MisplacedImportStatement (ErrorLocation name (getMetadata d))]
  pure defs
 where
  ds = dropWhile isImport defs
  es = dropWhile (not . isImport) ds
