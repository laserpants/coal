{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoStack (
  ProtoCompilerT (..),
  runProtoCompilerT,
  evalProtoCompilerT,
  protoOupdateSupplyC,
  insertBuildC,
  setCurrentPathC,
  setCurrentModuleC,
  protoOsetSubstitutionC,
  protoOgetCurrentBuildC,
  protoOinsertConstraintsC,
  protoOclearConstraintsC,
  protoOinsertAssumptionsC,
  protoOclearAssumptionsC,
  protoOclearTypeAnnotationParamsC,
  clearNameStoreC,
  insertNameC,
  insertNamesC,
  setNamesC,
  setTypeAnnotationParamsC,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Language
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Coal.ProtoCompiler.ProtoJournal (ProtoCompilerJournal (..))
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoModule
import Coal.TypeSystem.Substitution (Substitution)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWST, runRWST)
import Control.Monad.State (get, modify)
import Extras (Dictionary, Name, Over)

type ProtoCompilerStack m a o = ExceptT () (RWST () (ProtoCompilerJournal a) (ProtoCompilerState a) m) o

newtype ProtoCompilerT m a o = ProtoCompiler {protoOcompilerStack :: ProtoCompilerStack m a o}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader ()
    , MonadWriter (ProtoCompilerJournal a)
    , MonadState (ProtoCompilerState a)
    , MonadError ()
    , --    , MonadReader (CompilerEnvironment a)
      --    , MonadWriter (CompilerJournal a)
      --    , MonadError CompilerFailureMode
      MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

-- TODO
runProtoCompilerT :: (Monad m) => ProtoCompilerT m a o -> m (Either () o, ProtoCompilerState a, [()])
runProtoCompilerT com = do
  (c, s, w) <- runRWST (runExceptT (protoOcompilerStack com)) () initialProtoCompilerState
  pure (c, s, [])

-- TODO
evalProtoCompilerT :: (Monad m) => ProtoCompilerT m a o -> m (Either () o)
evalProtoCompilerT com = do
  (c, _, _) <- runProtoCompilerT com
  pure c

protoOupdateSupplyC :: (Monad m) => Int -> ProtoCompilerT m a ()
protoOupdateSupplyC supply = modify (overProtoCompilerSupply (const supply))

insertBuildC :: (Monad m) => ProtoBuild a -> ProtoCompilerT m a ()
insertBuildC ProtoBuild{..} = modify (overProtoCompilerModules (Environment.insert principalName ProtoBuild{..}))
 where
  principalName = principalPath protoObuildPath

setCurrentPathC :: (Monad m) => Path -> ProtoCompilerT m a ()
setCurrentPathC path = modify (overProtoCompilerCurrentPath (const path))

setCurrentModuleC :: (Monad m) => ProtoModule a s t -> ProtoCompilerT m a ()
setCurrentModuleC ProtoModule{..} = setCurrentPathC protoOmodulePath

protoOsetSubstitutionC :: (Monad m) => Substitution -> ProtoCompilerT m a ()
protoOsetSubstitutionC sub = modify (overProtoCompilerSubstitution (const sub))

protoOgetCurrentBuildC :: (Monad m) => ProtoCompilerT m a (ProtoBuild a)
protoOgetCurrentBuildC = do
  ProtoCompilerState{..} <- get
  case Environment.lookup (principalPath protoOcompilerCurrentPath) protoOcompilerModules of
    Nothing ->
      --      error "Implementation error"
      error (show (principalPath protoOcompilerCurrentPath))
    Just build ->
      return build

protoOinsertConstraintsC :: (Monad m) => [CompilerConstraint a] -> ProtoCompilerT m a ()
protoOinsertConstraintsC constraints = modify (overProtoCompilerConstraints (<> constraints))

protoOclearConstraintsC :: (Monad m) => ProtoCompilerT m a ()
protoOclearConstraintsC = modify (overProtoCompilerConstraints (const mempty))

protoOinsertAssumptionsC :: (Monad m) => [CompilerAssumption a] -> ProtoCompilerT m a ()
protoOinsertAssumptionsC assumptions = modify (overProtoCompilerAssumptions (<> assumptions))

protoOclearAssumptionsC :: (Monad m) => ProtoCompilerT m a ()
protoOclearAssumptionsC = modify (overProtoCompilerAssumptions (const mempty))

protoOclearTypeAnnotationParamsC :: (Monad m) => ProtoCompilerT m a ()
protoOclearTypeAnnotationParamsC = modify (overProtoCompilerTypeAnnotationParams (const mempty))

clearNameStoreC :: (Monad m) => ProtoCompilerT m a ()
clearNameStoreC = modify (overProtoCompilerNameStore (const mempty))

insertNameC :: (Monad m) => Name -> IndexedScheme -> ProtoCompilerT m a ()
insertNameC name scheme_ = modify (overProtoCompilerNameStore (Environment.insert name scheme_))

insertNamesC :: (Monad m) => [(Name, IndexedScheme)] -> ProtoCompilerT m a ()
insertNamesC names = modify (overProtoCompilerNameStore (Environment.insertMultiple names))

setNamesC :: (Monad m) => Environment IndexedScheme -> ProtoCompilerT m a ()
setNamesC names = modify (overProtoCompilerNameStore (const names))

setTypeAnnotationParamsC :: (Monad m) => Dictionary (a, TypeIndex Kind) -> ProtoCompilerT m a ()
setTypeAnnotationParamsC params = modify (overProtoCompilerTypeAnnotationParams (const params))
