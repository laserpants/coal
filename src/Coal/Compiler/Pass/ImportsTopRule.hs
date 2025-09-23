{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.ImportsTopRule (importsTopRulePass) where

import Coal.Ast.Metadata (HasMetadata (..), Metadata (..))
import Coal.Compiler.Journal
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.Language.Module.Definition (isImport)
import Control.Monad.Except
import Extra (Name)

importsTopRulePass :: (MonadIO m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
importsTopRulePass =
  Pass
    { passName = "ImportsTopRule"
    , runPass = pass
    }

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT Metadata m [Module Metadata Kind ()]
pass ms = do
  (ps, errs) <- listenErrors $ traverse (\m -> overModuleDefinitionsM (checkImports (modulePathName m)) m) ms
  unless (null errs) $
    throwError PreflightFailure
  pure ps

checkImports :: (Monad m) => Name -> [Definition Metadata k ()] -> CompilerT Metadata m [Definition Metadata k ()]
checkImports name defs = do
  case es of
    [] ->
      pure ()
    rs -> do
      forM_ (filter isImport rs) $
        \d ->
          tellErrors [MisplacedImportStatement name (getMetadata d)]
  pure defs
 where
  ds = dropWhile isImport defs
  es = dropWhile (not . isImport) ds
