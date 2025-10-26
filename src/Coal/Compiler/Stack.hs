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
  insertConstraintsC,
  insertAssumptionsC,
  updateSubstitutionC,
  clearConstraintsC,
  clearTypeAnnotationParamsC,
  updateSupply,
  updateSupplyC,
  insertSupplyC,
  insertTypeDefinitionsC,
  setVerbatimSourceC,
  setVerbatimSourceForC,
  getVerbatimSourceC,
  compilerReportConstraintsGenErrors,
  compilerReportSolverRuleViolations,
  compilerSetTypeAnnotationParams,
  setCompilerModuleC,
  setConfigExecutableNameC,
  setConfigGenerateDotFilesC,
)
where

import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (Supply (..))
import Coal.Compiler.Config (setConfigExecutableName, setConfigGenerateDotFiles)
import Coal.Compiler.Environment (CompilerEnvironment (..))
import Coal.Compiler.Error
import Coal.Compiler.Journal
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Module (Definition (..), Module (..), modulePathName)
import Coal.Language.Module.Definition (Path (..))
import Coal.TypeSystem
import Control.Monad.Except
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, gets, modify)
import Control.Monad.Writer (MonadWriter)
import Data.Text (Text)
import Extra (Dictionary, Name)

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

{-# INLINE runCompilerT #-}
runCompilerT :: (Monad m) => CompilerEnvironment -> CompilerT a m c -> m (Either CompilerFailureMode c, CompilerState a, [CompilerError a])
runCompilerT env com = do
  (c, s, w) <- runRWST (runExceptT (compilerStack com)) env initialCompilerState
  pure (c, s, compilerJournalErrors w)

{-# INLINE evalCompilerT #-}
evalCompilerT :: (Monad m) => CompilerEnvironment -> CompilerT a m c -> m (Either CompilerFailureMode c)
evalCompilerT env com = do
  (c, _, _) <- runCompilerT env com
  pure c

{-# INLINE compilerSetTypeAnnotationParams #-}
compilerSetTypeAnnotationParams :: (Monad m) => Dictionary (a, TypeIndex Kind) -> CompilerT a m ()
compilerSetTypeAnnotationParams params = modify (overCompilerTypeAnnotationParams (const params))

{-# INLINE compilerReportConstraintsGenErrors #-}
compilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> CompilerT a m ()
compilerReportConstraintsGenErrors errors = modify (overCompilerStateConstraintsGenErrors (<> errors))

{-# INLINE compilerReportSolverRuleViolations #-}
compilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> CompilerT a m ()
compilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

{-# INLINE insertSupplyC #-}
insertSupplyC :: (Monad m) => Int -> CompilerT a m ()
insertSupplyC = modify . overCompilerSupply . const

{-# INLINE insertNameC #-}
insertNameC :: (Monad m) => Name -> IndexedScheme -> CompilerT a m ()
insertNameC name scheme_ = modify (overCompilerNameStore (Environment.insert name scheme_))

{-# INLINE insertNamesC #-}
insertNamesC :: (Monad m) => [(Name, IndexedScheme)] -> CompilerT a m ()
insertNamesC names = modify (overCompilerNameStore (Environment.insertMultiple names))

{-# INLINE insertConstraintsC #-}
insertConstraintsC :: (Monad m) => [CompilerConstraint a] -> CompilerT a m ()
insertConstraintsC cs = modify (overCompilerConstraints (<> cs))

{-# INLINE clearConstraintsC #-}
clearConstraintsC :: (Monad m) => CompilerT a m ()
clearConstraintsC = modify (overCompilerConstraints (const mempty))

{-# INLINE clearTypeAnnotationParamsC #-}
clearTypeAnnotationParamsC :: (Monad m) => CompilerT a m ()
clearTypeAnnotationParamsC = modify (overCompilerTypeAnnotationParams (const mempty))

{-# INLINE insertAssumptionsC #-}
insertAssumptionsC :: (Monad m) => [CompilerAssumption a] -> CompilerT a m ()
insertAssumptionsC as = modify (overCompilerAssumptions (<> as))

{-# INLINE updateSupplyC #-}
updateSupplyC :: (Monad m) => Int -> CompilerT a m ()
updateSupplyC supply = modify (overCompilerSupply (const supply))

{-# INLINE updateSubstitutionC #-}
updateSubstitutionC :: (Monad m) => Substitution -> CompilerT a m ()
updateSubstitutionC sub = modify (overCompilerSubstitution (const sub))

{-# INLINE setVerbatimSourceC #-}
setVerbatimSourceC :: (Monad m) => Name -> Text -> CompilerT a m ()
setVerbatimSourceC name src = modify (overCompilerVerbatimSource (Environment.insert name src))

{-# INLINE setVerbatimSourceForC #-}
setVerbatimSourceForC :: (Monad m) => Module a k t -> Text -> CompilerT a m ()
setVerbatimSourceForC module_ = setVerbatimSourceC (modulePathName module_)

{-# INLINE getVerbatimSourceC #-}
getVerbatimSourceC :: (Monad m) => Name -> CompilerT a m Text
getVerbatimSourceC name = do
  s <- gets compilerVerbatimSource
  case Environment.lookup name s of
    Nothing ->
      error "Implementation error"
    Just src ->
      pure src

{-# INLINE insertTypeDefinitionsC #-}
insertTypeDefinitionsC :: (Monad m) => Name -> [Definition a Kind ()] -> CompilerT a m ()
insertTypeDefinitionsC name defs = modify (overCompilerTypeDefinitions (Environment.insert name defs))

{-# INLINE setCompilerModuleC #-}
setCompilerModuleC :: (Monad m) => Path -> CompilerT a m ()
setCompilerModuleC path = modify (overCompilerModule (const path))

setConfigExecutableNameC :: (Monad m) => FilePath -> CompilerT a m ()
setConfigExecutableNameC name = modify (overCompilerConfig (setConfigExecutableName name))

setConfigGenerateDotFilesC :: (Monad m) => Bool -> CompilerT a m ()
setConfigGenerateDotFilesC flag = modify (overCompilerConfig (setConfigGenerateDotFiles flag))
