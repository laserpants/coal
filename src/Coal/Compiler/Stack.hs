-- +
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Stack

The compiler monad stack and state management operations.

This module defines the core compiler monad transformer @CompilerT@, which
combines reader, writer, state, and error handling effects. It provides
operations for managing compilation state including builds, constraints,
assumptions, type information, and configuration.
-}
module Coal.Compiler.Stack (
  -- * Core types
  CompilerT (..),
  CompilerEnvironment (..),
  CompilerJournal (..),
  CompilerError (..),
  CompilerFailureMode (..),
  CompilerStack,
  ErrorLocation (..),

  -- * Monad runners
  runCompilerT,
  evalCompilerT,

  -- * Supply management
  updateSupplyC,

  -- * Build management
  insertBuildC,
  getBuildC,
  getCurrentBuildC,
  updateBuildC,
  updateCurrentBuildC,
  updateCurrentBuildPureC,
  setBitcodeC,
  setBuildSourceC,

  -- * Path and module management
  setCurrentPathC,
  setCurrentModuleC,

  -- * Constraints
  insertConstraintsC,
  clearConstraintsC,
  insertKindConstraintsC,
  clearKindConstraintsC,

  -- * Assumptions
  insertAssumptionsC,
  clearAssumptionsC,

  -- * Name store
  insertNameC,
  insertNamesC,
  setNamesC,
  clearNameStoreC,

  -- * Type annotations
  setTypeAnnotationParamsC,
  clearTypeAnnotationParamsC,

  -- * Type substitution
  setSubstitutionC,

  -- * Error reporting
  compilerReportConstraintsGenErrors,
  compilerReportKindConstraintsGenErrors,
  compilerReportSolverRuleViolations,
  compilerGetConstraintsGenErrorsC,
  compilerGetSolverRuleViolationsC,

  -- * Configuration
  setConfigC,
  setConfigExecutableNameC,
  setConfigGenerateDotFilesC,
  setConfigGenerateLLVMOutputC,

  -- * Source management
  getSourceC,
  setTouched,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (Build (..), setBuildBitcode)
import Coal.Compiler.Config (CompilerConfig, setConfigExecutableName, setConfigGenerateDotFiles, setConfigGenerateLLVMOutput)
import Coal.Compiler.Environment (CompilerEnvironment (..))
import Coal.Compiler.Error (CompilerError (..), CompilerFailureMode (..), ErrorLocation (..))
import Coal.Compiler.Journal (CompilerJournal (..))
import Coal.Compiler.State
import Coal.Language (IndexedScheme, Kind, TypeIndex)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.TypeSystem.Constraint.Generation.Error (ConstraintsGenError (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule)
import Coal.TypeSystem.Kind.Constraint (KindConstraint (..))
import Coal.TypeSystem.Kind.Error (KindError (..))
import Coal.TypeSystem.Substitution (Substitution)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT (..), MonadError, MonadIO, runExceptT)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWST, runRWST)
import Control.Monad.State (get, gets, modify)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Extras (Dictionary, Name)

-- ----------------------------------------------------------------------------
-- Core types and monad stack
-- ----------------------------------------------------------------------------

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

-- ----------------------------------------------------------------------------
-- Monad runners
-- ----------------------------------------------------------------------------

runCompilerT :: (Monad m) => CompilerEnvironment a -> CompilerT a m c -> m (Either CompilerFailureMode c, CompilerState a, [CompilerError a])
runCompilerT env com = do
  (c, s, w) <- runRWST (runExceptT (compilerStack com)) env initialCompilerState
  pure (c, s, compilerJournalErrors w)

evalCompilerT :: (Monad m) => CompilerEnvironment a -> CompilerT a m c -> m (Either CompilerFailureMode c)
evalCompilerT env com = do
  (c, _, _) <- runCompilerT env com
  pure c

-- ----------------------------------------------------------------------------
-- Supply management
-- ----------------------------------------------------------------------------

{-# INLINE updateSupplyC #-}
updateSupplyC :: (Monad m) => Int -> CompilerT a m ()
updateSupplyC supply = modify (overCompilerSupply (const supply))

-- ----------------------------------------------------------------------------
-- Build management
-- ----------------------------------------------------------------------------

-- | Typeclass for values that can be converted to build names.
class (Show p) => BuildName p where
  buildName :: p -> Name

instance BuildName Path where
  buildName = principalPath

instance BuildName Text where
  buildName = id

{-# INLINE insertBuildC #-}
insertBuildC :: (Monad m) => Build a -> CompilerT a m ()
insertBuildC Build{..} = modify (overCompilerModules (Environment.insert principalName Build{..}))
 where
  principalName = principalPath buildPath

{-# INLINE getBuildC #-}
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
      error $ "Internal error: No build found for current path " ++ show compilerCurrentPath
    Just build ->
      return build

updateBuildC :: (Monad m, BuildName p) => p -> (Build a -> CompilerT a m (Build a)) -> CompilerT a m ()
updateBuildC name update = do
  maybeBuild <- getBuildC name
  case maybeBuild of
    Nothing ->
      pure () -- Silently ignore missing builds (may be external/cached)
    Just build -> do
      newBuild <- update build
      modify (overCompilerModules (Environment.insert (buildName name) newBuild))

updateCurrentBuildC :: (Monad m) => (Build a -> CompilerT a m (Build a)) -> CompilerT a m ()
updateCurrentBuildC f = do
  CompilerState{compilerCurrentPath} <- get
  updateBuildC compilerCurrentPath f

{-# INLINE updateCurrentBuildPureC #-}
updateCurrentBuildPureC :: (Monad m) => (Build a -> Build a) -> CompilerT a m ()
updateCurrentBuildPureC f = updateCurrentBuildC (pure . f)

{-# INLINE setBitcodeC #-}
setBitcodeC :: (Monad m, BuildName p) => p -> ByteString -> CompilerT a m ()
setBitcodeC build bs = updateBuildC build (pure . setBuildBitcode bs)

{-# INLINE setBuildSourceC #-}
setBuildSourceC :: (Monad m, BuildName p) => p -> Text -> CompilerT a m ()
setBuildSourceC build source = modify (overCompilerSources (Environment.insert (buildName build) source))

-- ----------------------------------------------------------------------------
-- Path and module management
-- ----------------------------------------------------------------------------

{-# INLINE setCurrentPathC #-}
setCurrentPathC :: (Monad m) => Path -> CompilerT a m ()
setCurrentPathC path = modify (overCompilerCurrentPath (const path))

{-# INLINE setCurrentModuleC #-}
setCurrentModuleC :: (Monad m) => Module a s t -> CompilerT a m ()
setCurrentModuleC Module{..} = setCurrentPathC modulePath

-- ----------------------------------------------------------------------------
-- Constraints
-- ----------------------------------------------------------------------------

{-# INLINE insertConstraintsC #-}
insertConstraintsC :: (Monad m) => [CompilerConstraint a] -> CompilerT a m ()
insertConstraintsC constraints = modify (overCompilerConstraints (<> constraints))

{-# INLINE clearConstraintsC #-}
clearConstraintsC :: (Monad m) => CompilerT a m ()
clearConstraintsC = modify (overCompilerConstraints (const mempty))

{-# INLINE insertKindConstraintsC #-}
insertKindConstraintsC :: (Monad m) => [KindConstraint] -> CompilerT a m ()
insertKindConstraintsC constraints = modify (overCompilerKindConstraints (<> constraints))

{-# INLINE clearKindConstraintsC #-}
clearKindConstraintsC :: (Monad m) => CompilerT a m ()
clearKindConstraintsC = modify (overCompilerKindConstraints (const mempty))

-- ----------------------------------------------------------------------------
-- Assumptions
-- ----------------------------------------------------------------------------

{-# INLINE insertAssumptionsC #-}
insertAssumptionsC :: (Monad m) => [CompilerAssumption a] -> CompilerT a m ()
insertAssumptionsC assumptions = modify (overCompilerAssumptions (<> assumptions))

{-# INLINE clearAssumptionsC #-}
clearAssumptionsC :: (Monad m) => CompilerT a m ()
clearAssumptionsC = modify (overCompilerAssumptions (const mempty))

-- ----------------------------------------------------------------------------
-- Name store
-- ----------------------------------------------------------------------------

{-# INLINE insertNameC #-}
insertNameC :: (Monad m) => Name -> IndexedScheme -> CompilerT a m ()
insertNameC name scheme_ = modify (overCompilerNameStore (Environment.insert name scheme_))

{-# INLINE insertNamesC #-}
insertNamesC :: (Monad m) => [(Name, IndexedScheme)] -> CompilerT a m ()
insertNamesC names = modify (overCompilerNameStore (Environment.insertMultiple names))

{-# INLINE setNamesC #-}
setNamesC :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
setNamesC names = modify (overCompilerNameStore (const names))

{-# INLINE clearNameStoreC #-}
clearNameStoreC :: (Monad m) => CompilerT a m ()
clearNameStoreC = modify (overCompilerNameStore (const mempty))

-- ----------------------------------------------------------------------------
-- Type annotations
-- ----------------------------------------------------------------------------

{-# INLINE setTypeAnnotationParamsC #-}
setTypeAnnotationParamsC :: (Monad m) => Dictionary (a, TypeIndex Kind) -> CompilerT a m ()
setTypeAnnotationParamsC params = modify (overCompilerTypeAnnotationParams (const params))

{-# INLINE clearTypeAnnotationParamsC #-}
clearTypeAnnotationParamsC :: (Monad m) => CompilerT a m ()
clearTypeAnnotationParamsC = modify (overCompilerTypeAnnotationParams (const mempty))

-- ----------------------------------------------------------------------------
-- Type substitution
-- ----------------------------------------------------------------------------

{-# INLINE setSubstitutionC #-}
setSubstitutionC :: (Monad m) => Substitution -> CompilerT a m ()
setSubstitutionC sub = modify (overCompilerSubstitution (const sub))

-- ----------------------------------------------------------------------------
-- Error reporting
-- ----------------------------------------------------------------------------

{-# INLINE compilerReportConstraintsGenErrors #-}
compilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> CompilerT a m ()
compilerReportConstraintsGenErrors errors = modify (overCompilerConstraintsGenErrors (<> errors))

{-# INLINE compilerReportKindConstraintsGenErrors #-}
compilerReportKindConstraintsGenErrors :: (Monad m) => [KindError] -> CompilerT a m ()
compilerReportKindConstraintsGenErrors errors = modify (overCompilerKindConstraintsGenErrors (<> errors))

{-# INLINE compilerReportSolverRuleViolations #-}
compilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> CompilerT a m ()
compilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

{-# INLINE compilerGetConstraintsGenErrorsC #-}
compilerGetConstraintsGenErrorsC :: (Monad m, Eq a) => CompilerT a m [ConstraintsGenError a]
compilerGetConstraintsGenErrorsC = gets (nub . compilerConstraintsGenErrors)

{-# INLINE compilerGetSolverRuleViolationsC #-}
compilerGetSolverRuleViolationsC :: (Monad m, Eq a) => CompilerT a m [InferenceRule Kind a]
compilerGetSolverRuleViolationsC = gets (nub . compilerSolverRuleViolations)

-- ----------------------------------------------------------------------------
-- Configuration
-- ----------------------------------------------------------------------------

{-# INLINE setConfigC #-}
setConfigC :: (Monad m) => CompilerConfig -> CompilerT a m ()
setConfigC config = modify (overCompilerConfig (const config))

{-# INLINE setConfigExecutableNameC #-}
setConfigExecutableNameC :: (Monad m) => FilePath -> CompilerT a m ()
setConfigExecutableNameC name = modify (overCompilerConfig (setConfigExecutableName name))

{-# INLINE setConfigGenerateDotFilesC #-}
setConfigGenerateDotFilesC :: (Monad m) => Bool -> CompilerT a m ()
setConfigGenerateDotFilesC flag = modify (overCompilerConfig (setConfigGenerateDotFiles flag))

{-# INLINE setConfigGenerateLLVMOutputC #-}
setConfigGenerateLLVMOutputC :: (Monad m) => Bool -> CompilerT a m ()
setConfigGenerateLLVMOutputC flag = modify (overCompilerConfig (setConfigGenerateLLVMOutput flag))

-- ----------------------------------------------------------------------------
-- Source management
-- ----------------------------------------------------------------------------

getSourceC :: (Monad m) => Name -> CompilerT a m Text
getSourceC name = do
  s <- gets compilerSources
  pure (fromMaybe (error $ "Internal error: No source found for module " ++ show name) (Environment.lookup name s))

{-# INLINE setTouched #-}
setTouched :: (Monad m, BuildName p) => p -> CompilerT a m ()
setTouched build = modify (overCompilerTouched (Set.insert (buildName build)))
