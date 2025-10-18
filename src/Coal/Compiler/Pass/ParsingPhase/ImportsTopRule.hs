{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.ParsingPhase.ImportsTopRule (passImportsTopRule) where

import Coal.Ast.Metadata (HasMetadata (..), Metadata (..))
import Coal.Compiler.Journal
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.Language.Module.Definition (isDImport)
import Control.Monad.Except
import Extra (Name)

passImportsTopRule :: (MonadIO m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
passImportsTopRule =
  Pass
    { passName = "ImportsTopRule"
    , runPass = pass
    }

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT Metadata m [Module Metadata Kind ()]
pass ms = do
  (ps, errors) <- listenErrors $ traverse (\m -> overModuleDefinitionsM (checkImports (modulePathName m)) m) ms
  unless (null errors) $
    throwError PreflightFailure
  pure ps

checkImports :: (Monad m) => Name -> [Definition Metadata k ()] -> CompilerT Metadata m [Definition Metadata k ()]
checkImports name defs = do
  case es of
    [] ->
      pure ()
    rs -> do
      forM_ (filter isDImport rs) $
        \d ->
          tellErrors [MisplacedImportStatement (ErrorLocation name (getMetadata d))]
  pure defs
 where
  ds = dropWhile isDImport defs
  es = dropWhile (not . isDImport) ds
