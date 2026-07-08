{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhasePreflight.DetectInvalidExports

Detect invalid module export declarations.

This pass validates that every name listed in a module's export list refers
to a definition that actually exists within the module.  Exporting a name
that is not defined in the module is an error.

For example, this module would be rejected:

@
module Foo(baz) {
  fun hello() = "A"
}
@

because @baz@ is not defined anywhere in @Foo@.  Similarly, a type export
like @Foo.T(baz)@ requires both a type @T@ declared in the module and
constructors that belong to @T@.
-}
module Coal.Compiler.Pass.PhasePreflight.DetectInvalidExports (
  passDetectInvalidExports,
) where

import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Compiler.State (CompilerState (compilerCurrentPath))
import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Definition
import Coal.Language.Module
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, traverse_)

{- | Invalid export detection pass.

Validate that every name in a module's export list refers to a definition
that actually exists within the module.
-}
passDetectInvalidExports :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passDetectInvalidExports = mapPass $ Pass{runPass = traverse passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
passImpl m = do
  setCurrentModuleC m
  (_, errors) <- listenErrors (detectInvalidExports m)
  unless (null errors) $
    throwError PreflightFailure
  return m

class DetectInvalidExportsContext e where
  detectInvalidExports :: (Monad m) => e -> CompilerT Metadata m ()

instance (DetectInvalidExportsContext e) => DetectInvalidExportsContext [e] where
  detectInvalidExports = traverse_ detectInvalidExports

instance (DetectInvalidExportsContext e) => DetectInvalidExportsContext (NonEmpty e) where
  detectInvalidExports = traverse_ detectInvalidExports

instance DetectInvalidExportsContext (Module Metadata () ()) where
  detectInvalidExports module_@Module{..} = do
    case moduleExportList of
      ExportAll ->
        pure ()
      Exports exports ->
        traverse_ (checkExport module_) exports

checkExport :: (Monad m) => Module Metadata () () -> Export Metadata -> CompilerT Metadata m ()
checkExport Module{..} export =
  case export of
    NameExport loc name -> do
      unless (name `elem` definedNames) $ do
        currentPath <- gets compilerCurrentPath
        tellErrors [ExportNotInModule name modulePath (ErrorLocation (principalPath currentPath) loc)]
    TypeExport loc typeName memberNames -> do
      unless (typeName `elem` definedTypeNames) $ do
        currentPath <- gets compilerCurrentPath
        tellErrors [ExportNotInModule typeName modulePath (ErrorLocation (principalPath currentPath) loc)]
      -- Check that each exported member (constructor) belongs to the type
      traverse_ (checkTypeMember loc typeName) memberNames
 where
  -- All user-visible names defined at module level
  definedNames = concatMap definitionNames moduleDefinitions

  -- Names of types (DType and DTypeAlias)
  definedTypeNames = concatMap typeNames moduleDefinitions

  -- Type → its constructor names (from DType)
  typeConstructors = concatMap typeConstructorMap moduleDefinitions

  checkTypeMember :: (Monad m) => Metadata -> Name -> Name -> CompilerT Metadata m ()
  checkTypeMember loc typeName memberName = do
    unless (memberName `elem` lookupConstructors typeName) $ do
      currentPath <- gets compilerCurrentPath
      tellErrors [ExportNotInModule memberName modulePath (ErrorLocation (principalPath currentPath) loc)]

  lookupConstructors :: Name -> [Name]
  lookupConstructors typeName' =
    [c | (t, c) <- typeConstructors, t == typeName']

definitionNames :: Definition Metadata () () -> [Name]
definitionNames =
  \case
    DType _ name _ ->
      [name]
    DTypeAlias _ name _ ->
      [name]
    DFunction _ name _ ->
      [name]
    DFunctionGroup _ name _ ->
      [name]
    DFold _ name _ ->
      [name]
    DLet _ name _ ->
      [name]
    DTrait _ name _ ->
      [name]
    DInstance _ _ ->
      []
    DImport{} ->
      []
    DNamespaceImport{} ->
      []

typeNames :: Definition Metadata () () -> [Name]
typeNames =
  \case
    DType _ name _ ->
      [name]
    DTypeAlias _ name _ ->
      [name]
    _ ->
      []

typeConstructorMap :: Definition Metadata () () -> [(Name, Name)]
typeConstructorMap =
  \case
    DType _ name def ->
      [ (name, constructorName c)
      | c <- typeDefinitionConstructors def
      ]
    _ ->
      []