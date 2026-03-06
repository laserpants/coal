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
  protoOgetBuildC,
  protoOgetCurrentBuildC,
  protoOupdateBuildC,
  protoOupdateCurrentBuildC,
  protoOinsertConstraintsC,
  protoOclearConstraintsC,
  protoOinsertKindConstraintsC,
  protoOclearKindConstraintsC,
  protoOinsertAssumptionsC,
  protoOclearAssumptionsC,
  protoOclearTypeAnnotationParamsC,
  protoOclearNameStoreC,
  protoOinsertNameC,
  protoOinsertNamesC,
  setNamesC,
  setTypeAnnotationParamsC,
  protoOcompilerReportConstraintsGenErrors,
  protoOcompilerReportKindConstraintsGenErrors,
  protoOcompilerReportSolverRuleViolations,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Language
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Coal.ProtoCompiler.ProtoJournal (ProtoCompilerJournal (..))
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoModule
import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Coal.ProtoTypeSystem.Kind.Error (ProtoKindError (..))
import Coal.TypeSystem.Constraint.Generation.Error (ConstraintsGenError (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Coal.TypeSystem.Substitution (Substitution)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWST, runRWST)
import Control.Monad.State (get, gets, modify)
import Extras (Dictionary, Name)

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

protoOgetBuildC :: (Monad m) => Path -> ProtoCompilerT m a (Maybe (ProtoBuild a))
protoOgetBuildC path = do
  modules <- gets protoOcompilerModules
  pure (Environment.lookup (principalPath path) modules)

protoOgetCurrentBuildC :: (Monad m) => ProtoCompilerT m a (ProtoBuild a)
protoOgetCurrentBuildC = do
  ProtoCompilerState{..} <- get
  maybeBuild <- protoOgetBuildC protoOcompilerCurrentPath
  case maybeBuild of
    Nothing ->
      --      error "Implementation error"
      error (show (principalPath protoOcompilerCurrentPath))
    Just build ->
      return build

protoOupdateBuildC :: (Monad m) => Path -> (ProtoBuild a -> ProtoCompilerT m a (ProtoBuild a)) -> ProtoCompilerT m a ()
protoOupdateBuildC path f = do
  maybeBuild <- protoOgetBuildC path
  case maybeBuild of
    Nothing ->
      error "Implementation error"
    Just build -> do
      newBuild <- f build
      modify (overProtoCompilerModules (Environment.insert (principalPath path) newBuild))

protoOupdateCurrentBuildC :: (Monad m) => (ProtoBuild a -> ProtoCompilerT m a (ProtoBuild a)) -> ProtoCompilerT m a ()
protoOupdateCurrentBuildC f = do
  ProtoCompilerState{..} <- get
  protoOupdateBuildC protoOcompilerCurrentPath f

protoOinsertConstraintsC :: (Monad m) => [CompilerConstraint a] -> ProtoCompilerT m a ()
protoOinsertConstraintsC constraints = modify (overProtoCompilerConstraints (<> constraints))

protoOclearConstraintsC :: (Monad m) => ProtoCompilerT m a ()
protoOclearConstraintsC = modify (overProtoCompilerConstraints (const mempty))

protoOinsertKindConstraintsC :: (Monad m) => [ProtoKindConstraint] -> ProtoCompilerT m a ()
protoOinsertKindConstraintsC constraints = modify (overProtoCompilerKindConstraints (<> constraints))

protoOclearKindConstraintsC :: (Monad m) => ProtoCompilerT m a ()
protoOclearKindConstraintsC = modify (overProtoCompilerKindConstraints (const mempty))

protoOinsertAssumptionsC :: (Monad m) => [CompilerAssumption a] -> ProtoCompilerT m a ()
protoOinsertAssumptionsC assumptions = modify (overProtoCompilerAssumptions (<> assumptions))

protoOclearAssumptionsC :: (Monad m) => ProtoCompilerT m a ()
protoOclearAssumptionsC = modify (overProtoCompilerAssumptions (const mempty))

protoOclearTypeAnnotationParamsC :: (Monad m) => ProtoCompilerT m a ()
protoOclearTypeAnnotationParamsC = modify (overProtoCompilerTypeAnnotationParams (const mempty))

protoOclearNameStoreC :: (Monad m) => ProtoCompilerT m a ()
protoOclearNameStoreC = modify (overProtoCompilerNameStore (const mempty))

protoOinsertNameC :: (Monad m) => Name -> IndexedScheme -> ProtoCompilerT m a ()
protoOinsertNameC name scheme_ = modify (overProtoCompilerNameStore (Environment.insert name scheme_))

protoOinsertNamesC :: (Monad m) => [(Name, IndexedScheme)] -> ProtoCompilerT m a ()
protoOinsertNamesC names = modify (overProtoCompilerNameStore (Environment.insertMultiple names))

setNamesC :: (Monad m) => Environment IndexedScheme -> ProtoCompilerT m a ()
setNamesC names = modify (overProtoCompilerNameStore (const names))

setTypeAnnotationParamsC :: (Monad m) => Dictionary (a, TypeIndex Kind) -> ProtoCompilerT m a ()
setTypeAnnotationParamsC params = modify (overProtoCompilerTypeAnnotationParams (const params))

{-# INLINE protoOcompilerReportConstraintsGenErrors #-}
protoOcompilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> ProtoCompilerT m a ()
protoOcompilerReportConstraintsGenErrors errors = modify (overProtoCompilerConstraintsGenErrors (<> errors))

{-# INLINE protoOcompilerReportKindConstraintsGenErrors #-}
protoOcompilerReportKindConstraintsGenErrors :: (Monad m) => [ProtoKindError] -> ProtoCompilerT m a ()
protoOcompilerReportKindConstraintsGenErrors errors = modify (overProtoCompilerKindConstraintsGenErrors (<> errors))

{-# INLINE protoOcompilerReportSolverRuleViolations #-}
protoOcompilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> ProtoCompilerT m a ()
protoOcompilerReportSolverRuleViolations errors = modify (overProtoCompilerSolverRuleViolations (<> errors))
