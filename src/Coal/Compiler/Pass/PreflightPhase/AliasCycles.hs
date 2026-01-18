{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.AliasCycles (passAliasCycles) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (BuildUnit (..), Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, traverse_)

passAliasCycles :: (MonadIO m) => Pass Metadata m [BuildUnit (Module Metadata Kind ())] [BuildUnit (Module Metadata Kind ())]
passAliasCycles = mapPass $ Pass{runPass = traverse (withCurrentModuleC_ detectAliasCycles)}

class TransformContext e where
  detectAliasCycles :: (Monad m) => e -> CompilerT Metadata m ()

instance (TransformContext e) => TransformContext [e] where
  detectAliasCycles = traverse_ detectAliasCycles

instance (TransformContext e) => TransformContext (NonEmpty e) where
  detectAliasCycles = traverse_ detectAliasCycles

instance (TransformContext e) => TransformContext (Maybe e) where
  detectAliasCycles = traverse_ detectAliasCycles

instance TransformContext (Module Metadata Kind ()) where
  detectAliasCycles =
    \case
      Module _ _ o -> do
        traverse_ detectAliasCycles o

instance TransformContext (Definition Metadata k ()) where
  detectAliasCycles =
    \case
      DTypeAlias loc name (AliasDefinition _ t) ->
        detectCycles loc name t
      _ ->
        pure ()

detectCycles :: (Monad m) => Metadata -> Name -> ParameterizedType -> CompilerT Metadata m ()
detectCycles loc name =
  \case
    TApplication _ t1 t2 -> do
      detectCycles loc name t1
      detectCycles loc name t2
    TArrow t1 t2 -> do
      detectCycles loc name t1
      detectCycles loc name t2
    TConstructor _ con
      | name == con -> do
          path <- gets compilerCurrentModule
          tellErrors [TypeAliasCycle name (ErrorLocation (principalPath path) loc)]
          throwError PreflightFailure
    TRecord t ->
      detectCycles loc name t
    TRow r ->
      detectRowCycles loc name r
    TAlias _ ts t -> do
      traverse_ (detectCycles loc name) ts
      detectCycles loc name t
    _ ->
      pure ()

detectRowCycles :: (Monad m) => Metadata -> Name -> Row Parameter () ParameterizedType -> CompilerT Metadata m ()
detectRowCycles loc name =
  \case
    RExtend _ t r -> do
      detectCycles loc name t
      detectRowCycles loc name r
    _ ->
      pure ()
