{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.ImportsTopRule (importsTopRulePass) where

import Coal.Ast.Metadata (HasMetadata (..), Metadata (..))
import Coal.Compiler.Journal
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language.Module
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO)
import Data.Tuple.Extra (secondM)
import Extra (forM_)

importsTopRulePass :: (MonadIO m) => Pass Metadata m [ModuleBundle] [ModuleBundle]
importsTopRulePass =
  Pass
    { passName = "ImportsTopRule"
    , runPass = pass
    }

pass :: (Monad m) => [ModuleBundle] -> CompilerT Metadata m [ModuleBundle]
pass ms = do
  ps <- traverse (secondM (overModuleDefinitionsM checkImports)) ms
  throwError PreflightFailure
  pure ps

checkImports :: (Monad m) => [Definition Metadata k ()] -> CompilerT Metadata m [Definition Metadata k ()]
checkImports defs = do
  case es of
    [] ->
      pure ()
    rs -> do
      forM_ (filter isImport rs) $
        \d ->
          tellErrors [MisplacedImportStatement (getMetadata d)]
  pure defs
 where
  ds = dropWhile isImport defs
  es = dropWhile (not . isImport) ds

isImport :: Definition a k t -> Bool
isImport =
  \case
    DImport{} ->
      True
    _ ->
      False
