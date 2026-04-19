{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.State
Description: Compiler state management and accessor functions

This module defines the CompilerState data structure, which maintains all
mutable state during compilation including type inference state, constraints,
error tracking, and module management.
-}
module Coal.Compiler.State (
  -- * Type aliases
  CompilerConstraint,
  CompilerAssumption,

  -- * Compiler state
  CompilerState (..),
  initialCompilerState,

  -- * Supply operations
  overCompilerSupply,

  -- * Configuration
  overCompilerConfig,

  -- * Module and source management
  overCompilerModules,
  overCompilerSources,
  overCompilerTouched,
  overCompilerModuleWithPath,

  -- * Path operations
  overCompilerCurrentPath,

  -- * Type system
  overCompilerSubstitution,
  overCompilerNameStore,
  overCompilerTypeAnnotationParams,

  -- * Constraint management
  overCompilerConstraints,
  overCompilerKindConstraints,
  overCompilerAssumptions,

  -- * Error tracking
  overCompilerConstraintsGenErrors,
  overCompilerKindConstraintsGenErrors,
  overCompilerSolverRuleViolations,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (Supply (..))
import Coal.Compiler.Build (Build (..))
import Coal.Compiler.Config (CompilerConfig (..), defaultConfig)
import Coal.Language
import Coal.Language.Module.Path (Path (..), emptyPath, principalPath)
import Coal.TypeSystem.Constraint (Constraint)
import Coal.TypeSystem.Constraint.Assumption (Assumption)
import Coal.TypeSystem.Constraint.Generation.Error (ConstraintsGenError (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Coal.TypeSystem.Kind.Constraint (KindConstraint (..))
import Coal.TypeSystem.Kind.Error (KindError (..))
import Coal.TypeSystem.Substitution (Substitution)
import Data.Set (Set)
import Data.Text (Text)
import Extras (Dictionary, Name, Over)

-- -----------------------------------------------------------------------------

-- * Type aliases

-- | Constraint type specialized for the compiler with inference rules
type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

-- | Assumption type specialized for the compiler
type CompilerAssumption a = Assumption a IndexedType

-- -----------------------------------------------------------------------------

-- * Compiler state

{- | Central compiler state containing all mutable compilation state.

Fields:
- 'compilerSupply': Fresh variable supply for generating unique identifiers
- 'compilerConfig': Compiler configuration settings
- 'compilerModules': Environment of compiled modules and their build information
- 'compilerSources': Source code text for each module
- 'compilerTouched': Set of modules that have been modified
- 'compilerCurrentPath': Currently compiling module path
- 'compilerSubstitution': Type variable substitutions from unification
- 'compilerNameStore': Type schemes for all named values
- 'compilerConstraints': Type constraints generated during inference
- 'compilerAssumptions': Type assumptions for constraint solving
- 'compilerTypeAnnotationParams': Type parameters from explicit type annotations
- 'compilerKindConstraints': Kind constraints for type-level inference
- 'compilerConstraintsGenErrors': Errors from constraint generation phase
- 'compilerKindConstraintsGenErrors': Errors from kind inference
- 'compilerSolverRuleViolations': Inference rules that failed during solving
-}
data CompilerState a = CompilerState
  { compilerSupply :: Int
  , compilerConfig :: CompilerConfig
  , compilerModules :: Environment (Build a)
  , compilerSources :: Environment Text
  , compilerTouched :: Set Name
  , compilerCurrentPath :: Path
  , compilerSubstitution :: Substitution
  , compilerNameStore :: Environment IndexedScheme
  , compilerConstraints :: [CompilerConstraint a]
  , compilerAssumptions :: [CompilerAssumption a]
  , compilerTypeAnnotationParams :: Dictionary (a, TypeIndex Kind)
  , compilerKindConstraints :: [KindConstraint]
  , compilerConstraintsGenErrors :: [ConstraintsGenError a]
  , compilerKindConstraintsGenErrors :: [KindError]
  , compilerSolverRuleViolations :: [InferenceRule Kind a]
  }
  deriving (Show, Eq, Ord)

-- | Supply instance enables fresh variable generation
instance Supply (CompilerState a) where
  updateSupply = overCompilerSupply
  getSupply = compilerSupply

-- -----------------------------------------------------------------------------

-- * Initialization

-- | Create an initial compiler state with all fields set to default values
initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compilerSupply = 0
    , compilerConfig = defaultConfig
    , compilerModules = mempty
    , compilerSources = mempty
    , compilerTouched = mempty
    , compilerCurrentPath = emptyPath
    , compilerSubstitution = mempty
    , compilerNameStore = mempty
    , compilerConstraints = mempty
    , compilerAssumptions = mempty
    , compilerTypeAnnotationParams = mempty
    , compilerKindConstraints = mempty
    , compilerConstraintsGenErrors = []
    , compilerKindConstraintsGenErrors = []
    , compilerSolverRuleViolations = []
    }

-- -----------------------------------------------------------------------------

-- * Supply operations

-- | Update the fresh variable supply
{-# INLINE overCompilerSupply #-}
overCompilerSupply :: Over (CompilerState a) Int
overCompilerSupply fn CompilerState{..} =
  CompilerState
    { compilerSupply = fn compilerSupply
    , ..
    }

-- -----------------------------------------------------------------------------

-- * Configuration

-- | Update the compiler configuration
{-# INLINE overCompilerConfig #-}
overCompilerConfig :: Over (CompilerState a) CompilerConfig
overCompilerConfig fn CompilerState{..} =
  CompilerState
    { compilerConfig = fn compilerConfig
    , ..
    }

-- -----------------------------------------------------------------------------

-- * Module and source management

-- | Update the environment of compiled modules
{-# INLINE overCompilerModules #-}
overCompilerModules :: Over (CompilerState a) (Environment (Build a))
overCompilerModules fn CompilerState{..} =
  CompilerState
    { compilerModules = fn compilerModules
    , ..
    }

-- | Update a specific module's build information by path
{-# INLINE overCompilerModuleWithPath #-}
overCompilerModuleWithPath :: Path -> Over (CompilerState a) (Build a)
overCompilerModuleWithPath path fn CompilerState{..} =
  CompilerState
    { compilerModules = Environment.adjust fn (principalPath path) compilerModules
    , ..
    }

-- | Update the environment of source code texts
{-# INLINE overCompilerSources #-}
overCompilerSources :: Over (CompilerState a) (Environment Text)
overCompilerSources fn CompilerState{..} =
  CompilerState
    { compilerSources = fn compilerSources
    , ..
    }

-- | Update the set of modified modules
{-# INLINE overCompilerTouched #-}
overCompilerTouched :: Over (CompilerState a) (Set Name)
overCompilerTouched fn CompilerState{..} =
  CompilerState
    { compilerTouched = fn compilerTouched
    , ..
    }

-- -----------------------------------------------------------------------------

-- * Path operations

-- | Update the currently compiling module path
{-# INLINE overCompilerCurrentPath #-}
overCompilerCurrentPath :: Over (CompilerState a) Path
overCompilerCurrentPath fn CompilerState{..} =
  CompilerState
    { compilerCurrentPath = fn compilerCurrentPath
    , ..
    }

-- -----------------------------------------------------------------------------

-- * Type system

-- | Update the type variable substitution from unification
{-# INLINE overCompilerSubstitution #-}
overCompilerSubstitution :: Over (CompilerState a) Substitution
overCompilerSubstitution fn CompilerState{..} =
  CompilerState
    { compilerSubstitution = fn compilerSubstitution
    , ..
    }

-- | Update the environment mapping names to their type schemes
{-# INLINE overCompilerNameStore #-}
overCompilerNameStore :: Over (CompilerState a) (Environment IndexedScheme)
overCompilerNameStore fn CompilerState{..} =
  CompilerState
    { compilerNameStore = fn compilerNameStore
    , ..
    }

-- -----------------------------------------------------------------------------

-- * Constraint management

-- | Update the list of type constraints generated during inference
{-# INLINE overCompilerConstraints #-}
overCompilerConstraints :: Over (CompilerState a) [CompilerConstraint a]
overCompilerConstraints fn CompilerState{..} =
  CompilerState
    { compilerConstraints = fn compilerConstraints
    , ..
    }

-- | Update the list of kind constraints for type-level inference
{-# INLINE overCompilerKindConstraints #-}
overCompilerKindConstraints :: Over (CompilerState a) [KindConstraint]
overCompilerKindConstraints fn CompilerState{..} =
  CompilerState
    { compilerKindConstraints = fn compilerKindConstraints
    , ..
    }

-- | Update the list of type assumptions for constraint solving
{-# INLINE overCompilerAssumptions #-}
overCompilerAssumptions :: Over (CompilerState a) [CompilerAssumption a]
overCompilerAssumptions fn CompilerState{..} =
  CompilerState
    { compilerAssumptions = fn compilerAssumptions
    , ..
    }

-- | Update the dictionary of type annotation parameters
{-# INLINE overCompilerTypeAnnotationParams #-}
overCompilerTypeAnnotationParams :: Over (CompilerState a) (Dictionary (a, TypeIndex Kind))
overCompilerTypeAnnotationParams fn CompilerState{..} =
  CompilerState
    { compilerTypeAnnotationParams = fn compilerTypeAnnotationParams
    , ..
    }

-- -----------------------------------------------------------------------------

-- * Error tracking

-- | Update the list of errors from constraint generation
{-# INLINE overCompilerConstraintsGenErrors #-}
overCompilerConstraintsGenErrors :: Over (CompilerState a) [ConstraintsGenError a]
overCompilerConstraintsGenErrors fn CompilerState{..} =
  CompilerState
    { compilerConstraintsGenErrors = fn compilerConstraintsGenErrors
    , ..
    }

-- | Update the list of errors from kind inference
{-# INLINE overCompilerKindConstraintsGenErrors #-}
overCompilerKindConstraintsGenErrors :: Over (CompilerState a) [KindError]
overCompilerKindConstraintsGenErrors fn CompilerState{..} =
  CompilerState
    { compilerKindConstraintsGenErrors = fn compilerKindConstraintsGenErrors
    , ..
    }

-- | Update the list of inference rules that failed during constraint solving
{-# INLINE overCompilerSolverRuleViolations #-}
overCompilerSolverRuleViolations :: Over (CompilerState a) [InferenceRule Kind a]
overCompilerSolverRuleViolations fn CompilerState{..} =
  CompilerState
    { compilerSolverRuleViolations = fn compilerSolverRuleViolations
    , ..
    }
