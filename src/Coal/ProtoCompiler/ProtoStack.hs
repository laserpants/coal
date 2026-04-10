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
  protoOsetNamesC,
  setTypeAnnotationParamsC,
  protoOsetBitcodeC,
  protoOsetBuildSourceC,
  protoOcompilerReportConstraintsGenErrors,
  protoOcompilerReportKindConstraintsGenErrors,
  protoOcompilerReportSolverRuleViolations,
  setConfigC,
  setConfigExecutableNameC,
  setConfigGenerateDotFilesC,
  setConfigGenerateLLVMOutputC,
  getSourceC,
  toBeRecompiled,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Config
import Coal.Language
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..), setBuildBitcode, setBuildSource)
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
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace
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

class (Show p) => BuildName p where
  buildName :: p -> Name

instance BuildName Path where
  buildName = principalPath

instance BuildName Text where
  buildName = id

protoOgetBuildC :: (Monad m, BuildName p) => p -> ProtoCompilerT m a (Maybe (ProtoBuild a))
protoOgetBuildC path = do
  modules <- gets protoOcompilerModules
  pure (Environment.lookup (buildName path) modules)

protoOgetCurrentBuildC :: (Monad m) => ProtoCompilerT m a (ProtoBuild a)
protoOgetCurrentBuildC = do
  ProtoCompilerState{..} <- get
  maybeBuild <- protoOgetBuildC protoOcompilerCurrentPath
  case maybeBuild of
    Nothing ->
      error "Implementation error"
    --      error (show (principalPath protoOcompilerCurrentPath))
    Just build ->
      return build

protoOupdateBuildC :: (Monad m, BuildName p) => p -> (ProtoBuild a -> ProtoCompilerT m a (ProtoBuild a)) -> ProtoCompilerT m a ()
protoOupdateBuildC name f = do
  maybeBuild <- protoOgetBuildC name
  case maybeBuild of
    Nothing ->
      --      error (show name)        -- ????
      pure ()
    Just build -> do
      newBuild <- f build
      modify (overProtoCompilerModules (Environment.insert (buildName name) newBuild))

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

protoOsetNamesC :: (Monad m) => Environment IndexedScheme -> ProtoCompilerT m a ()
protoOsetNamesC names = modify (overProtoCompilerNameStore (const names))

setTypeAnnotationParamsC :: (Monad m) => Dictionary (a, TypeIndex Kind) -> ProtoCompilerT m a ()
setTypeAnnotationParamsC params = modify (overProtoCompilerTypeAnnotationParams (const params))

protoOsetBitcodeC :: (Monad m, BuildName p) => p -> ByteString -> ProtoCompilerT m a ()
protoOsetBitcodeC build bs = protoOupdateBuildC build (pure . setBuildBitcode bs)

protoOsetBuildSourceC :: (Monad m, BuildName p) => p -> Text -> ProtoCompilerT m a ()
protoOsetBuildSourceC build source = protoOupdateBuildC build (pure . setBuildSource source)

{-# INLINE protoOcompilerReportConstraintsGenErrors #-}
protoOcompilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> ProtoCompilerT m a ()
protoOcompilerReportConstraintsGenErrors errors = modify (overProtoCompilerConstraintsGenErrors (<> errors))

{-# INLINE protoOcompilerReportKindConstraintsGenErrors #-}
protoOcompilerReportKindConstraintsGenErrors :: (Monad m) => [ProtoKindError] -> ProtoCompilerT m a ()
protoOcompilerReportKindConstraintsGenErrors errors = modify (overProtoCompilerKindConstraintsGenErrors (<> errors))

{-# INLINE protoOcompilerReportSolverRuleViolations #-}
protoOcompilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> ProtoCompilerT m a ()
protoOcompilerReportSolverRuleViolations errors = modify (overProtoCompilerSolverRuleViolations (<> errors))

setConfigC :: (Monad m) => CompilerConfig -> ProtoCompilerT m a ()
setConfigC config = modify (overProtoCompilerConfig (const config))

setConfigExecutableNameC :: (Monad m) => FilePath -> ProtoCompilerT m a ()
setConfigExecutableNameC name = modify (overProtoCompilerConfig (setConfigExecutableName name))

setConfigGenerateDotFilesC :: (Monad m) => Bool -> ProtoCompilerT m a ()
setConfigGenerateDotFilesC flag = modify (overProtoCompilerConfig (setConfigGenerateDotFiles flag))

setConfigGenerateLLVMOutputC :: (Monad m) => Bool -> ProtoCompilerT m a ()
setConfigGenerateLLVMOutputC flag = modify (overProtoCompilerConfig (setConfigGenerateLLVMOutput flag))

getSourceC :: (Monad m) => Name -> ProtoCompilerT m a Text
getSourceC name = do
  s <- gets protoOcompilerSources
  pure (fromMaybe (error "Implementation error") (Environment.lookup name s))

toBeRecompiled :: (Monad m, BuildName p) => p -> ProtoCompilerT m a ()
toBeRecompiled build = modify (overProtoCompilerToBeRecompiled (Set.insert (buildName build)))
