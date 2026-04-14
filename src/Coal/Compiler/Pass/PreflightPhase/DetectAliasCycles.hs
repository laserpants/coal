-- +
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.DetectAliasCycles (
  passDetectAliasCycles,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, traverse_)

passDetectAliasCycles :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passDetectAliasCycles = mapPass $ Pass{runPass = traverse passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
passImpl m = do
  setCurrentModuleC m
  (_, errors) <- listenErrors (detectCycles m)
  unless (null errors) $
    throwError PreflightFailure
  return m

class AliasContext e where
  detectCycles :: (Monad m) => e -> CompilerT Metadata m ()

instance (AliasContext e) => AliasContext [e] where
  detectCycles = traverse_ detectCycles

instance (AliasContext e) => AliasContext (NonEmpty e) where
  detectCycles = traverse_ detectCycles

instance (AliasContext e) => AliasContext (Maybe e) where
  detectCycles = traverse_ detectCycles

instance AliasContext (Module Metadata () ()) where
  detectCycles =
    \case
      Module _ _ o -> do
        traverse_ detectCycles o

instance AliasContext (Definition Metadata () ()) where
  detectCycles =
    \case
      DTypeAlias loc name (AliasDefinition _ t) ->
        detectCyclesInType loc name t
      _ ->
        pure ()

detectCyclesInType :: (Monad m) => Metadata -> Name -> ParameterizedType -> CompilerT Metadata m ()
detectCyclesInType loc name =
  \case
    TApplication _ t1 t2 -> do
      detectCyclesInType loc name t1
      detectCyclesInType loc name t2
    TArrow t1 t2 -> do
      detectCyclesInType loc name t1
      detectCyclesInType loc name t2
    TConstructor _ con
      | name == con -> do
          path <- gets compilerCurrentPath
          tellErrors [TypeAliasCycle name (ErrorLocation (principalPath path) loc)]
    TRecord t ->
      detectCyclesInType loc name t
    TRow r ->
      detectCyclesInRow loc name r
    TAlias _ ts t -> do
      traverse_ (detectCyclesInType loc name) ts
      detectCyclesInType loc name t
    _ ->
      pure ()

detectCyclesInRow :: (Monad m) => Metadata -> Name -> Row Parameter () ParameterizedType -> CompilerT Metadata m ()
detectCyclesInRow loc name =
  \case
    RExtend _ t r -> do
      detectCyclesInType loc name t
      detectCyclesInRow loc name r
    _ ->
      pure ()
