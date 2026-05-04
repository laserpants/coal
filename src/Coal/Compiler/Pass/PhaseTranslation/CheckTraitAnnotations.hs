{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.PhaseTranslation.CheckTraitAnnotations (
  passCheckTraitAnnotations,
) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Debug.Trace
import Extras (forM_)

passCheckTraitAnnotations :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passCheckTraitAnnotations = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl m = do
  setCurrentModuleC m
  checkTraitAnnotations m
  return m

checkTraitAnnotations :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m ()
checkTraitAnnotations Module{..} = do
  names <- gets compilerNameStore
  forM_ moduleDefinitions $
    \case
      DFunction{} ->
        pure ()
      DLet loc name LetDefinition{..} -> do
        case Environment.lookup name names of
          Nothing ->
            error "TODO"
          Just s -> do
            pure ()
        -- traceShowM name
        -- traceShowM s
        -- traceShowM letDefinitionAnnotation
        pure ()
      DInstance loc InstanceDefinition{..} ->
        forM_ instanceDefinitionImplementations $
          \case
            DFunction{} ->
              pure ()
            DLet a name LetDefinition{..} ->
              pure ()
            _ ->
              pure ()
      _ ->
        pure ()
