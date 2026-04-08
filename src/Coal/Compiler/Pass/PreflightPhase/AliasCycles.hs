{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.AliasCycles (passAliasCycles) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, traverse_)

passAliasCycles :: (MonadIO m) => Pass Metadata m [BuildUnit (ProtoModule Metadata () ())] [BuildUnit (ProtoModule Metadata () ())]
passAliasCycles = mapPass $ Pass{runPass = traverse impl}

impl :: (MonadIO m) => ProtoModule Metadata () () -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata () ())
impl mm = do
  --  let mm = toProtoModule [] m
  lift $ setCurrentPathC (protoOmodulePath mm)
  detectAliasCycles mm
  return mm

-- impl = traverse (withCurrentModuleC_ detectAliasCycles)

class TransformContext e where
  detectAliasCycles :: (Monad m) => e -> CompilerT Metadata (ProtoCompilerT m Metadata) ()

instance (TransformContext e) => TransformContext [e] where
  detectAliasCycles = traverse_ detectAliasCycles

instance (TransformContext e) => TransformContext (NonEmpty e) where
  detectAliasCycles = traverse_ detectAliasCycles

instance (TransformContext e) => TransformContext (Maybe e) where
  detectAliasCycles = traverse_ detectAliasCycles

-- instance TransformContext (Module Metadata Kind ()) where
--  detectAliasCycles =
--    \case
--      Module _ _ o -> do
--        traverse_ detectAliasCycles o
--
-- instance TransformContext (Definition Metadata k ()) where
--  detectAliasCycles =
--    \case
--      DTypeAlias loc name (AliasDefinition _ t) ->
--        detectCycles loc name t
--      _ ->
--        pure ()

instance TransformContext (ProtoModule Metadata () ()) where
  detectAliasCycles =
    \case
      ProtoModule _ _ o -> do
        traverse_ detectAliasCycles o

instance TransformContext (ProtoDefinition Metadata () ()) where
  detectAliasCycles =
    \case
      ProtoDTypeAlias loc name (ProtoAliasDefinition _ t) ->
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
      detectCyclesInRow loc name r
    TAlias _ ts t -> do
      traverse_ (detectCycles loc name) ts
      detectCycles loc name t
    _ ->
      pure ()

detectCyclesInRow :: (Monad m) => Metadata -> Name -> Row Parameter () ParameterizedType -> CompilerT Metadata m ()
detectCyclesInRow loc name =
  \case
    RExtend _ t r -> do
      detectCycles loc name t
      detectCyclesInRow loc name r
    _ ->
      pure ()
