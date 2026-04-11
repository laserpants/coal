{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Stack (
  CompilerT (..),
  CompilerEnvironment (..),
  CompilerJournal (..),
  CompilerError (..),
  CompilerFailureMode (..),
  CompilerStack,
  --  CompilerState (..),
  --  CompilerConstraint,
  --  CompilerAssumption,
  ErrorLocation (..),
  runCompilerT,
  evalCompilerT,
  updateSupply,
  --  setVerbatimSourceC,
  --  setVerbatimSourceForC,
  --  getVerbatimSourceC,
  --  setCompilerCurrentModuleC,
  --  setConfigExecutableNameC,
  --  setConfigGenerateDotFilesC,
  --  setConfigGenerateLLVMOutputC,
  --  setConfigC,
  -- insertModuleC,
  --  insertCurrentModuleC,
  --  getCurrentBuildC,
  --  updateCurrentBuildC,
  --  updateBuildC,
  --  withCurrentModuleC_,
  --  withCurrentModuleC,
  -- setBitcodeC,
  --  insertFreshModule,
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
import Coal.Common.Supply (Supply (..))
import Coal.Compiler.Build (Build (..), setBuildBitcode, setBuildSource)
import Coal.Compiler.Config
import Coal.Compiler.Environment (CompilerEnvironment (..))
import Coal.Compiler.Error
import Coal.Compiler.Journal (CompilerJournal (..))
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Module
import Coal.Language.Module.Path
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.TypeSystem.Constraint.Generation.Error (ConstraintsGenError (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Coal.TypeSystem.Kind.Constraint (KindConstraint (..))
import Coal.TypeSystem.Kind.Error (KindError (..))
import Coal.TypeSystem.Substitution (Substitution)
import Control.Monad.Catch
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (..), MonadError, MonadIO, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWST, runRWST)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, get, gets, modify)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter)
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace
import Extras (Dictionary, Name, fromMaybe)

type CompilerStack a m c = ExceptT CompilerFailureMode (RWST (CompilerEnvironment a) (CompilerJournal a) (CompilerState a) m) c

newtype CompilerT a m c = Compiler {compilerStack :: CompilerStack a m c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (CompilerEnvironment a)
    , MonadWriter (CompilerJournal a)
    , MonadState (CompilerState a)
    , MonadError CompilerFailureMode
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadTrans (CompilerT a) where
  lift = Compiler . lift . lift

runCompilerT :: (Monad m) => CompilerEnvironment a -> CompilerT a m c -> m (Either CompilerFailureMode c, CompilerState a, [CompilerError a])
runCompilerT env com = do
  (c, s, w) <- runRWST (runExceptT (compilerStack com)) env initialCompilerState
  pure (c, s, compilerJournalErrors w)

evalCompilerT :: (Monad m) => CompilerEnvironment a -> CompilerT a m c -> m (Either CompilerFailureMode c)
evalCompilerT env com = do
  (c, _, _) <- runCompilerT env com
  pure c

protoOupdateSupplyC :: (Monad m) => Int -> CompilerT a m ()
protoOupdateSupplyC supply = modify (overCompilerSupply (const supply))

insertBuildC :: (Monad m) => Build a -> CompilerT a m ()
insertBuildC Build{..} = modify (overCompilerModules (Environment.insert principalName Build{..}))
 where
  principalName = principalPath protoObuildPath

setCurrentPathC :: (Monad m) => Path -> CompilerT a m ()
setCurrentPathC path = modify (overCompilerCurrentPath (const path))

setCurrentModuleC :: (Monad m) => Module a s t -> CompilerT a m ()
setCurrentModuleC Module{..} = setCurrentPathC protoOmodulePath

protoOsetSubstitutionC :: (Monad m) => Substitution -> CompilerT a m ()
protoOsetSubstitutionC sub = modify (overCompilerSubstitution (const sub))

class (Show p) => BuildName p where
  buildName :: p -> Name

instance BuildName Path where
  buildName = principalPath

instance BuildName Text where
  buildName = id

protoOgetBuildC :: (Monad m, BuildName p) => p -> CompilerT a m (Maybe (Build a))
protoOgetBuildC path = do
  modules <- gets protoOcompilerModules
  pure (Environment.lookup (buildName path) modules)

protoOgetCurrentBuildC :: (Monad m) => CompilerT a m (Build a)
protoOgetCurrentBuildC = do
  CompilerState{..} <- get
  maybeBuild <- protoOgetBuildC protoOcompilerCurrentPath
  case maybeBuild of
    Nothing ->
      error "Implementation error"
    --      error (show (principalPath protoOcompilerCurrentPath))
    Just build ->
      return build

protoOupdateBuildC :: (Monad m, BuildName p) => p -> (Build a -> CompilerT a m (Build a)) -> CompilerT a m ()
protoOupdateBuildC name f = do
  maybeBuild <- protoOgetBuildC name
  case maybeBuild of
    Nothing ->
      --      error (show name)        -- ????
      pure ()
    Just build -> do
      newBuild <- f build
      modify (overCompilerModules (Environment.insert (buildName name) newBuild))

protoOupdateCurrentBuildC :: (Monad m) => (Build a -> CompilerT a m (Build a)) -> CompilerT a m ()
protoOupdateCurrentBuildC f = do
  CompilerState{..} <- get
  protoOupdateBuildC protoOcompilerCurrentPath f

protoOinsertConstraintsC :: (Monad m) => [CompilerConstraint a] -> CompilerT a m ()
protoOinsertConstraintsC constraints = modify (overCompilerConstraints (<> constraints))

protoOclearConstraintsC :: (Monad m) => CompilerT a m ()
protoOclearConstraintsC = modify (overCompilerConstraints (const mempty))

protoOinsertKindConstraintsC :: (Monad m) => [KindConstraint] -> CompilerT a m ()
protoOinsertKindConstraintsC constraints = modify (overCompilerKindConstraints (<> constraints))

protoOclearKindConstraintsC :: (Monad m) => CompilerT a m ()
protoOclearKindConstraintsC = modify (overCompilerKindConstraints (const mempty))

protoOinsertAssumptionsC :: (Monad m) => [CompilerAssumption a] -> CompilerT a m ()
protoOinsertAssumptionsC assumptions = modify (overCompilerAssumptions (<> assumptions))

protoOclearAssumptionsC :: (Monad m) => CompilerT a m ()
protoOclearAssumptionsC = modify (overCompilerAssumptions (const mempty))

protoOclearTypeAnnotationParamsC :: (Monad m) => CompilerT a m ()
protoOclearTypeAnnotationParamsC = modify (overCompilerTypeAnnotationParams (const mempty))

protoOclearNameStoreC :: (Monad m) => CompilerT a m ()
protoOclearNameStoreC = modify (overCompilerNameStore (const mempty))

protoOinsertNameC :: (Monad m) => Name -> IndexedScheme -> CompilerT a m ()
protoOinsertNameC name scheme_ = modify (overCompilerNameStore (Environment.insert name scheme_))

protoOinsertNamesC :: (Monad m) => [(Name, IndexedScheme)] -> CompilerT a m ()
protoOinsertNamesC names = modify (overCompilerNameStore (Environment.insertMultiple names))

protoOsetNamesC :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
protoOsetNamesC names = modify (overCompilerNameStore (const names))

setTypeAnnotationParamsC :: (Monad m) => Dictionary (a, TypeIndex Kind) -> CompilerT a m ()
setTypeAnnotationParamsC params = modify (overCompilerTypeAnnotationParams (const params))

protoOsetBitcodeC :: (Monad m, BuildName p) => p -> ByteString -> CompilerT a m ()
protoOsetBitcodeC build bs = protoOupdateBuildC build (pure . setBuildBitcode bs)

protoOsetBuildSourceC :: (Monad m, BuildName p) => p -> Text -> CompilerT a m ()
protoOsetBuildSourceC build source = protoOupdateBuildC build (pure . setBuildSource source)

{-# INLINE protoOcompilerReportConstraintsGenErrors #-}
protoOcompilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> CompilerT a m ()
protoOcompilerReportConstraintsGenErrors errors = modify (overCompilerConstraintsGenErrors (<> errors))

{-# INLINE protoOcompilerReportKindConstraintsGenErrors #-}
protoOcompilerReportKindConstraintsGenErrors :: (Monad m) => [KindError] -> CompilerT a m ()
protoOcompilerReportKindConstraintsGenErrors errors = modify (overCompilerKindConstraintsGenErrors (<> errors))

{-# INLINE protoOcompilerReportSolverRuleViolations #-}
protoOcompilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> CompilerT a m ()
protoOcompilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

setConfigC :: (Monad m) => CompilerConfig -> CompilerT a m ()
setConfigC config = modify (overCompilerConfig (const config))

setConfigExecutableNameC :: (Monad m) => FilePath -> CompilerT a m ()
setConfigExecutableNameC name = modify (overCompilerConfig (setConfigExecutableName name))

setConfigGenerateDotFilesC :: (Monad m) => Bool -> CompilerT a m ()
setConfigGenerateDotFilesC flag = modify (overCompilerConfig (setConfigGenerateDotFiles flag))

setConfigGenerateLLVMOutputC :: (Monad m) => Bool -> CompilerT a m ()
setConfigGenerateLLVMOutputC flag = modify (overCompilerConfig (setConfigGenerateLLVMOutput flag))

getSourceC :: (Monad m) => Name -> CompilerT a m Text
getSourceC name = do
  s <- gets protoOcompilerSources
  pure (fromMaybe (error "Implementation error") (Environment.lookup name s))

toBeRecompiled :: (Monad m, BuildName p) => p -> CompilerT a m ()
toBeRecompiled build = modify (overCompilerToBeRecompiled (Set.insert (buildName build)))
