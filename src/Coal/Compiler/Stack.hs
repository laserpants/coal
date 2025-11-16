{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Stack (
  CompilerT (..),
  CompilerEnvironment (..),
  CompilerJournal (..),
  CompilerError (..),
  CompilerFailureMode (..),
  CompilerStack,
  CompilerState (..),
  CompilerConstraint,
  CompilerAssumption,
  ErrorLocation (..),
  runCompilerT,
  evalCompilerT,
  insertNameC,
  insertNamesC,
  insertGlobalNamesC,
  insertConstraintsC,
  insertAssumptionsC,
  clearAssumptionsC,
  clearNameStoreC,
  clearConstraintsC,
  clearTypeAnnotationParamsC,
  updateSubstitutionC,
  updateSupply,
  updateSupplyC,
  insertSupplyC,
  setVerbatimSourceC,
  setVerbatimSourceForC,
  getVerbatimSourceC,
  compilerReportConstraintsGenErrors,
  compilerReportSolverRuleViolations,
  compilerSetTypeAnnotationParams,
  setCompilerCurrentModuleC,
  setConfigExecutableNameC,
  setConfigGenerateDotFilesC,
  setConfigGenerateLLVMOutputC,
  setConfigC,
  insertModuleC,
) where

import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (Supply (..))
import Coal.Compiler.Config
import Coal.Compiler.Environment (CompilerEnvironment (..))
import Coal.Compiler.Error
import Coal.Compiler.Journal
import Coal.Compiler.Module.Bundle (ModuleBundle)
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Module (Module (..), modulePathName)
import Coal.Language.Module.Definition (Path (..))
import Coal.TypeSystem
import Control.Monad.Except
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, gets, modify)
import Control.Monad.Writer (MonadWriter)
import Data.Text (Text)
import Extras (Dictionary, Name)

type CompilerStack a m c = ExceptT CompilerFailureMode (RWST CompilerEnvironment (CompilerJournal a) (CompilerState a) m) c

newtype CompilerT a m c = Compiler {compilerStack :: CompilerStack a m c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CompilerEnvironment
    , MonadWriter (CompilerJournal a)
    , MonadState (CompilerState a)
    , MonadError CompilerFailureMode
    , MonadIO
    )

runCompilerT :: (Monad m) => CompilerEnvironment -> CompilerT a m c -> m (Either CompilerFailureMode c, CompilerState a, [CompilerError a])
runCompilerT env com = do
  (c, s, w) <- runRWST (runExceptT (compilerStack com)) env initialCompilerState
  pure (c, s, compilerJournalErrors w)

evalCompilerT :: (Monad m) => CompilerEnvironment -> CompilerT a m c -> m (Either CompilerFailureMode c)
evalCompilerT env com = do
  (c, _, _) <- runCompilerT env com
  pure c

compilerSetTypeAnnotationParams :: (Monad m) => Dictionary (a, TypeIndex Kind) -> CompilerT a m ()
compilerSetTypeAnnotationParams params = modify (overCompilerTypeAnnotationParams (const params))

compilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> CompilerT a m ()
compilerReportConstraintsGenErrors errors = modify (overCompilerStateConstraintsGenErrors (<> errors))

compilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> CompilerT a m ()
compilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

insertSupplyC :: (Monad m) => Int -> CompilerT a m ()
insertSupplyC = modify . overCompilerSupply . const

insertNameC :: (Monad m) => Name -> IndexedScheme -> CompilerT a m ()
insertNameC name scheme_ = modify (overCompilerNameStore (Environment.insert name scheme_))

insertNamesC :: (Monad m) => [(Name, IndexedScheme)] -> CompilerT a m ()
insertNamesC names = modify (overCompilerNameStore (Environment.insertMultiple names))

insertGlobalNamesC :: (Monad m) => Name -> Environment IndexedScheme -> CompilerT a m ()
insertGlobalNamesC name env = modify (overCompilerGlobalNames (Environment.insert name env))

insertConstraintsC :: (Monad m) => [CompilerConstraint a] -> CompilerT a m ()
insertConstraintsC cs = modify (overCompilerConstraints (<> cs))

clearConstraintsC :: (Monad m) => CompilerT a m ()
clearConstraintsC = modify (overCompilerConstraints (const mempty))

clearTypeAnnotationParamsC :: (Monad m) => CompilerT a m ()
clearTypeAnnotationParamsC = modify (overCompilerTypeAnnotationParams (const mempty))

clearAssumptionsC :: (Monad m) => CompilerT a m ()
clearAssumptionsC = modify (overCompilerAssumptions (const []))

clearNameStoreC :: (Monad m) => CompilerT a m ()
clearNameStoreC = modify (overCompilerNameStore (const mempty))

insertAssumptionsC :: (Monad m) => [CompilerAssumption a] -> CompilerT a m ()
insertAssumptionsC as = modify (overCompilerAssumptions (<> as))

updateSupplyC :: (Monad m) => Int -> CompilerT a m ()
updateSupplyC supply = modify (overCompilerSupply (const supply))

updateSubstitutionC :: (Monad m) => Substitution -> CompilerT a m ()
updateSubstitutionC sub = modify (overCompilerSubstitution (const sub))

setVerbatimSourceC :: (Monad m) => Name -> Text -> CompilerT a m ()
setVerbatimSourceC name src = modify (overCompilerVerbatimSource (Environment.insert name src))

setVerbatimSourceForC :: (Monad m) => Module a k t -> Text -> CompilerT a m ()
setVerbatimSourceForC module_ = setVerbatimSourceC (modulePathName module_)

getVerbatimSourceC :: (Monad m) => Name -> CompilerT a m Text
getVerbatimSourceC name = do
  s <- gets compilerVerbatimSource
  case Environment.lookup name s of
    Nothing ->
      error "Implementation error"
    Just src ->
      pure src

setCompilerCurrentModuleC :: (Monad m) => Path -> CompilerT a m ()
setCompilerCurrentModuleC path = modify (overCompilerCurrentModule (const path))

setConfigExecutableNameC :: (Monad m) => FilePath -> CompilerT a m ()
setConfigExecutableNameC name = modify (overCompilerConfig (setConfigExecutableName name))

setConfigGenerateDotFilesC :: (Monad m) => Bool -> CompilerT a m ()
setConfigGenerateDotFilesC flag = modify (overCompilerConfig (setConfigGenerateDotFiles flag))

setConfigGenerateLLVMOutputC :: (Monad m) => Bool -> CompilerT a m ()
setConfigGenerateLLVMOutputC flag = modify (overCompilerConfig (setConfigGenerateLLVMOutput flag))

setConfigC :: (Monad m) => CompilerConfig -> CompilerT a m ()
setConfigC config = modify (overCompilerConfig (const config))

insertModuleC :: (Monad m) => Name -> ModuleBundle a -> CompilerT a m ()
insertModuleC name bundle = modify (overCompilerModules (Environment.insert name bundle))
