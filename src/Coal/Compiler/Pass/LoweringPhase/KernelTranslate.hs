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
import Coal.Language.Module.Definition (Import (..))
import Control.Monad.IO.Class
import Extras (Name, for, (<.>))

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
      let env = collectImports build defs

      insertQualifiedNames env $
        withModuleName name $
          Kernel.Module
            name
            (Environment.elems env)
            . concat
            <$> traverse translateDefinition defs
     where
      name = principalPath path

collectImports :: ModuleBuild a -> [Definition a k t] -> Environment Name
collectImports build = Environment.fromList . concatMap (imports build)

imports :: ModuleBuild a -> Definition a k t -> [(Name, Name)]
imports ModuleBuild{..} =
  \case
    DImport _ path names_ ->
      concat . for names_ $
        \case
          (NameImport _ name) ->
            [(name, principalPath path <.> name)]
          (TypeImport _ name ["*"]) ->
            case Environment.lookup name moduleTypeConstructors of
              Nothing ->
                error "TODO"
              Just TypeConstructorInfo{..} ->
                [(name_, principalPath path <.> name_) | name_ <- typeConstructorInfoDataConstructors]
          (TypeImport _ _ ctors) ->
            [(ctor, principalPath path <.> ctor) | ctor <- ctors]
          (CotypeImport _ name ["*"]) ->
            case Environment.lookup name moduleCotypeConstructors of
              Nothing ->
                error "TODO"
              Just CotypeConstructorInfo{..} ->
                [(name_, principalPath path <.> name_) | name_ <- cotypeConstructorInfoDataAccessors]
          (CotypeImport _ _ xsors) ->
            [(xsor, principalPath path <.> xsor) | xsor <- xsors]
          (TraitImport _ name ["*"]) ->
            case Environment.lookup name moduleTraits of
              Nothing ->
                error "TODO"
              Just TraitInfo{..} ->
                [(name_, principalPath path <.> name_) | name_ <- Environment.names traitInfoEntries]
          (TraitImport _ _ entries) ->
            [(entry, principalPath path <.> entry) | entry <- entries]
    _ ->
      []
