{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Stack (
  CompilerT (..),
  CompilerEnvironment (..),
  CompilerState (..),
  CompilerConstraint,
  CompilerAssumption,
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
  setSourceTextC,
  compilerReportConstraintsGenErrors,
  compilerReportSolverRuleViolations,
  compilerSetTypeAnnotationParams,
)
where

import Coal.Common.Supply (Supply (..))
import Coal.Compiler.Environment (CompilerEnvironment (..))
import Coal.Compiler.State
import Coal.Language
import Coal.TypeSystem
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, modify)
import Data.Text (Text)
import Extra (Dictionary, Name, (<$$$>))
import Coal.Language.Module (Definition (..))

import qualified Coal.Common.Environment as Environment

type CompilerStack a m c = RWST CompilerEnvironment () (CompilerState a) m c

newtype CompilerT a m c = Compiler {compilerStack :: CompilerStack a m c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CompilerEnvironment
    , MonadState (CompilerState a)
    , MonadIO
    )

{-# INLINE runCompilerT #-}
runCompilerT :: (Monad m) => CompilerEnvironment -> CompilerT a m c -> m (c, CompilerState a)
runCompilerT env com = do
  (c, s, _) <- runRWST (compilerStack com) env initialCompilerState
  pure (c, s)

{-# INLINE evalCompilerT #-}
evalCompilerT :: (Monad m) => CompilerEnvironment -> CompilerT a m c -> m c
evalCompilerT = fst <$$$> runCompilerT

{-# INLINE compilerReportConstraintsGenErrors #-}
compilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> CompilerT a m ()
compilerReportConstraintsGenErrors errors = modify (overCompilerStateConstraintsGenErrors (<> errors))

{-# INLINE compilerSetTypeAnnotationParams #-}
compilerSetTypeAnnotationParams :: (Monad m) => Dictionary (a, TypeIndex Kind) -> CompilerT a m ()
compilerSetTypeAnnotationParams params = modify (overCompilerTypeAnnotationParams (const params))

{-# INLINE compilerReportSolverRuleViolations #-}
compilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> CompilerT a m ()
compilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

{-# INLINE insertSupplyC #-}
insertSupplyC :: (Monad m) => Int -> CompilerT a m ()
insertSupplyC = modify . overCompilerSupply . const

{-# INLINE insertNameC #-}
insertNameC :: (Monad m) => Name -> IndexedScheme -> CompilerT a m ()
insertNameC name scheme = modify (overCompilerNameStore (Environment.insert name scheme))

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
insertAssumptionsC :: (Monad m) => [CompilerAssumption] -> CompilerT a m ()
insertAssumptionsC as = modify (overCompilerAssumptions (<> as))

{-# INLINE updateSupplyC #-}
updateSupplyC :: (Monad m) => Int -> CompilerT a m ()
updateSupplyC supply = modify (overCompilerSupply (const supply))

{-# INLINE updateSubstitutionC #-}
updateSubstitutionC :: (Monad m) => Substitution -> CompilerT a m ()
updateSubstitutionC sub = modify (overCompilerSubstitution (const sub))

{-# INLINE setSourceTextC #-}
setSourceTextC :: (Monad m) => Text -> CompilerT a m ()
setSourceTextC src = modify (overCompilerSourceText (const src))

{-# INLINE insertTypeDefinitionsC #-}
insertTypeDefinitionsC :: (Monad m) => Name -> [Definition a Kind ()] -> CompilerT a m ()
insertTypeDefinitionsC name defs = modify (overCompilerTypeDefinitions (Environment.insert name defs))
