{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.LoweringPhase.KernelTranslate (passKernelTranslate) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Kernel.Environment (insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.TranslateDefinition (translateDefinition)
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language as Kernel
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Module
import Control.Monad.IO.Class
import Control.Monad.State (gets)
import Extras (Name, (<.>))
import Extras.Control.Monad (concatForM)

passKernelTranslate :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
passKernelTranslate =
  Pass
    { passName = "KernelTranslate"
    , runPass = pass
    }

pass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
pass =
  \case
    Module path _ defs -> do
      setCompilerCurrentModuleC path
      build <- getCurrentBuildC
      env <- collectImports build defs

      insertQualifiedNames env $
        withModuleName name $
          Kernel.Module
            name
            (Environment.elems env)
            . concat
            <$> traverse translateDefinition defs
     where
      name = principalPath path

collectImports :: (Monad m) => ModuleBuild a -> [Definition a k t] -> CompilerT Metadata m (Environment Name)
collectImports build defs = do
  xs <- concatForM defs (qualImports build)
  pure (Environment.fromList xs)

importedModule :: (Monad m) => Path -> CompilerT a m (ModuleBuild a)
importedModule path = do
  env <- gets compilerModules
  case Environment.lookup (principalPath path) env of
    Nothing ->
      error "Implementation error"
    Just build ->
      return build

qualImports :: (Monad m) => ModuleBuild a -> Definition a k t -> CompilerT Metadata m [(Name, Name)]
qualImports ModuleBuild{..} =
  \case
    DImport _ path names_ ->
      concatForM names_ $
        \case
          (ImportName _ name) ->
            pure [(name, principalPath path <.> name)]
          (ImportType _ name ["*"]) ->
            case Environment.lookup name moduleTypeConstructors of
              Nothing ->
                error "TODO"
              Just TypeConstructorEntry{..} ->
                pure [(name_, principalPath path <.> name_) | name_ <- typeConstructorEntryDataConstructors]
          (ImportType _ _ ctors) ->
            pure [(ctor, principalPath path <.> ctor) | ctor <- ctors]
          (ImportCotype _ name ["*"]) ->
            case Environment.lookup name moduleCotypeConstructors of
              Nothing ->
                error "TODO"
              Just CotypeConstructorEntry{..} ->
                pure [(name_, principalPath path <.> name_) | name_ <- cotypeConstructorEntryDataAccessors]
          (ImportCotype _ _ xsors) ->
            pure [(xsor, principalPath path <.> xsor) | xsor <- xsors]
          (ImportTrait _ name ["*"]) ->
            case Environment.lookup name moduleTraits of
              Nothing ->
                error "TODO"
              Just TraitEntry{..} ->
                pure [(name_, principalPath path <.> name_) | name_ <- Environment.names traitEntryEntries]
          (ImportTrait _ _ entries) ->
            pure [(entry, principalPath path <.> entry) | entry <- entries]
    DQualifiedImport _ path -> do
      build <- importedModule path
      concatForM (exportedNames build) $
        \case
          NFunction name _ ->
            pure [(qualified name path, qualified name path)]
          NConstant name _ ->
            pure [(qualified name path, qualified name path)]
          NFold name _ ->
            pure [(qualified name path, qualified name path)]
          NUnfold name _ ->
            pure [(qualified name path, qualified name path)]
          NDataConstructor name _ ->
            pure [(qualified name path, qualified name path)]
          NCodataAccessor name _ ->
            pure [(qualified name path, qualified name path)]
          _ ->
            pure []
    _ ->
      pure []
