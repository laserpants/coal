{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhasePreflight.DetectMisplacedImportStatements

Detect @import@ statements that are not at the top of the module.

This pass enforces the convention that all @import@ statements must appear at
the beginning of a module, before any other definitions.

For example, this is valid:

@
module MyModule

import List(concat, head, tail)
import String(is_empty)

fun my_function() = ...
@

But this would be flagged as an error:

@
module MyModule

fun my_function() = ...

import List(concat, head, tail)  // Error: import after definition
@

The pass reports all misplaced @import@ statements, requiring them to be moved
to the top of the module.
-}
module Coal.Compiler.Pass.PhasePreflight.DetectMisplacedImportStatements (
  passDetectMisplacedImportStatements,
) where

import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.HasMetadata (HasMetadata (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
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

{- | Misplaced import detection pass.

Validate that all @import@ statements appear at the top of the module before any
other definitions. Report errors for any import statements found after
non-import definitions, enforcing a consistent module structure.
-}
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
