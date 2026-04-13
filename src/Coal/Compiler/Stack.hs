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
  updateSupplyC,
  insertBuildC,
  setCurrentPathC,
  setCurrentModuleC,
  setSubstitutionC,
  getBuildC,
  getCurrentBuildC,
  updateBuildC,
  updateCurrentBuildC,
  updateCurrentBuildPureC,
  insertConstraintsC,
  clearConstraintsC,
  insertKindConstraintsC,
  clearKindConstraintsC,
  insertAssumptionsC,
  clearAssumptionsC,
  clearTypeAnnotationParamsC,
  clearNameStoreC,
  insertNameC,
  insertNamesC,
  setNamesC,
  setTypeAnnotationParamsC,
  setBitcodeC,
  setBuildSourceC,
  compilerReportConstraintsGenErrors,
  compilerReportKindConstraintsGenErrors,
  compilerReportSolverRuleViolations,
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
import Coal.Compiler.Build (Build (..), setBuildBitcode)
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

updateSupplyC :: (Monad m) => Int -> CompilerT a m ()
updateSupplyC supply = modify (overCompilerSupply (const supply))

insertBuildC :: (Monad m) => Build a -> CompilerT a m ()
insertBuildC Build{..} = modify (overCompilerModules (Environment.insert principalName Build{..}))
 where
  principalName = principalPath buildPath

setCurrentPathC :: (Monad m) => Path -> CompilerT a m ()
setCurrentPathC path = modify (overCompilerCurrentPath (const path))

setCurrentModuleC :: (Monad m) => Module a s t -> CompilerT a m ()
setCurrentModuleC Module{..} = setCurrentPathC modulePath

setSubstitutionC :: (Monad m) => Substitution -> CompilerT a m ()
setSubstitutionC sub = modify (overCompilerSubstitution (const sub))

class (Show p) => BuildName p where
  buildName :: p -> Name

instance BuildName Path where
  buildName = principalPath

instance BuildName Text where
  buildName = id

getBuildC :: (Monad m, BuildName p) => p -> CompilerT a m (Maybe (Build a))
getBuildC path = do
  modules <- gets compilerModules
  pure (Environment.lookup (buildName path) modules)

getCurrentBuildC :: (Monad m) => CompilerT a m (Build a)
getCurrentBuildC = do
  CompilerState{..} <- get
  maybeBuild <- getBuildC compilerCurrentPath
  case maybeBuild of
    Nothing ->
      error "Implementation error"
    --      error (show (principalPath compilerCurrentPath))
    Just build ->
      return build

updateBuildC :: (Monad m, BuildName p) => p -> (Build a -> CompilerT a m (Build a)) -> CompilerT a m ()
updateBuildC name update = do
  maybeBuild <- getBuildC name
  case maybeBuild of
    Nothing ->
      -- error (show name)        -- ????
      pure ()
    Just build -> do
      newBuild <- update build
      modify (overCompilerModules (Environment.insert (buildName name) newBuild))

updateCurrentBuildC :: (Monad m) => (Build a -> CompilerT a m (Build a)) -> CompilerT a m ()
updateCurrentBuildC f = do
  CompilerState{..} <- get
  updateBuildC compilerCurrentPath f

updateCurrentBuildPureC :: (Monad m) => (Build a -> Build a) -> CompilerT a m ()
updateCurrentBuildPureC f = updateCurrentBuildC (pure . f)

insertConstraintsC :: (Monad m) => [CompilerConstraint a] -> CompilerT a m ()
insertConstraintsC constraints = modify (overCompilerConstraints (<> constraints))

clearConstraintsC :: (Monad m) => CompilerT a m ()
clearConstraintsC = modify (overCompilerConstraints (const mempty))

insertKindConstraintsC :: (Monad m) => [KindConstraint] -> CompilerT a m ()
insertKindConstraintsC constraints = modify (overCompilerKindConstraints (<> constraints))

clearKindConstraintsC :: (Monad m) => CompilerT a m ()
clearKindConstraintsC = modify (overCompilerKindConstraints (const mempty))

insertAssumptionsC :: (Monad m) => [CompilerAssumption a] -> CompilerT a m ()
insertAssumptionsC assumptions = modify (overCompilerAssumptions (<> assumptions))

clearAssumptionsC :: (Monad m) => CompilerT a m ()
clearAssumptionsC = modify (overCompilerAssumptions (const mempty))

clearTypeAnnotationParamsC :: (Monad m) => CompilerT a m ()
clearTypeAnnotationParamsC = modify (overCompilerTypeAnnotationParams (const mempty))

clearNameStoreC :: (Monad m) => CompilerT a m ()
clearNameStoreC = modify (overCompilerNameStore (const mempty))

insertNameC :: (Monad m) => Name -> IndexedScheme -> CompilerT a m ()
insertNameC name scheme_ = modify (overCompilerNameStore (Environment.insert name scheme_))

insertNamesC :: (Monad m) => [(Name, IndexedScheme)] -> CompilerT a m ()
insertNamesC names = modify (overCompilerNameStore (Environment.insertMultiple names))

setNamesC :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
setNamesC names = modify (overCompilerNameStore (const names))

setTypeAnnotationParamsC :: (Monad m) => Dictionary (a, TypeIndex Kind) -> CompilerT a m ()
setTypeAnnotationParamsC params = modify (overCompilerTypeAnnotationParams (const params))

setBitcodeC :: (Monad m, BuildName p) => p -> ByteString -> CompilerT a m ()
setBitcodeC build bs = updateBuildC build (pure . setBuildBitcode bs)

setBuildSourceC :: (Monad m, BuildName p) => p -> Text -> CompilerT a m ()
setBuildSourceC build source = modify (overCompilerSources (Environment.insert (buildName build) source))

{-# INLINE compilerReportConstraintsGenErrors #-}
compilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> CompilerT a m ()
compilerReportConstraintsGenErrors errors = modify (overCompilerConstraintsGenErrors (<> errors))

{-# INLINE compilerReportKindConstraintsGenErrors #-}
compilerReportKindConstraintsGenErrors :: (Monad m) => [KindError] -> CompilerT a m ()
compilerReportKindConstraintsGenErrors errors = modify (overCompilerKindConstraintsGenErrors (<> errors))

{-# INLINE compilerReportSolverRuleViolations #-}
compilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> CompilerT a m ()
compilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

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
  s <- gets compilerSources
  pure (fromMaybe (error "Implementation error") (Environment.lookup name s))

toBeRecompiled :: (Monad m, BuildName p) => p -> CompilerT a m ()
toBeRecompiled build = modify (overCompilerToBeRecompiled (Set.insert (buildName build)))
