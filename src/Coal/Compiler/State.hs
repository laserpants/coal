{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.State (
  CompilerState (..),
  CompilerAssumption,
  CompilerConstraint,
  overCompilerSubstitution,
  overCompilerSupply,
  overCompilerAssumptions,
  overCompilerConstraints,
  overCompilerNameStore,
  overCompilerSolverRuleViolations,
  overCompilerTypeAnnotationParams,
  overCompilerStateConstraintsGenErrors,
  overCompilerVerbatimSource,
  overCompilerCurrentModule,
  overCompilerConfig,
  overCompilerModules,
  initialCompilerState,
) where

import Coal.Common.Environment (Environment (..))
import Coal.Common.Supply (Supply (..))
import Coal.Compiler.Build (ModuleBuild)
import Coal.Compiler.Config (CompilerConfig (..), defaultConfig)
import Coal.Language
import Coal.Language.Module.Definition (Path (..))
import Coal.TypeSystem
import Data.Text (Text)
import Extras (Dictionary, Over)

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

type CompilerAssumption a = Assumption a IndexedType

data CompilerState a = CompilerState
  { compilerSupply :: Int
  , compilerNameStore :: Environment IndexedScheme
  , compilerCurrentModule :: Path
  , compilerSubstitution :: Substitution
  , compilerConstraints :: [CompilerConstraint a]
  , compilerConstraintsGenErrors :: [ConstraintsGenError a]
  , compilerSolverRuleViolations :: [InferenceRule Kind a]
  , compilerAssumptions :: [CompilerAssumption a]
  , compilerTypeAnnotationParams :: Dictionary (a, TypeIndex Kind)
  , compilerVerbatimSource :: Environment Text
  , compilerConfig :: CompilerConfig
  , compilerModules :: Environment (ModuleBuild a)
  }
  deriving (Show, Eq, Ord, Read)

instance Supply (CompilerState a) where
  updateSupply = overCompilerSupply
  getSupply = compilerSupply

{-# INLINE overCompilerNameStore #-}
overCompilerNameStore :: Over (CompilerState a) (Environment IndexedScheme)
overCompilerNameStore fn CompilerState{..} =
  CompilerState
    { compilerNameStore = fn compilerNameStore
    , ..
    }

{-# INLINE overCompilerCurrentModule #-}
overCompilerCurrentModule :: Over (CompilerState a) Path
overCompilerCurrentModule fn CompilerState{..} =
  CompilerState
    { compilerCurrentModule = fn compilerCurrentModule
    , ..
    }

{-# INLINE overCompilerSupply #-}
overCompilerSupply :: Over (CompilerState a) Int
overCompilerSupply fn CompilerState{..} =
  CompilerState
    { compilerSupply = fn compilerSupply
    , ..
    }

{-# INLINE overCompilerSubstitution #-}
overCompilerSubstitution :: Over (CompilerState a) Substitution
overCompilerSubstitution fn CompilerState{..} =
  CompilerState
    { compilerSubstitution = fn compilerSubstitution
    , ..
    }

{-# INLINE overCompilerConstraints #-}
overCompilerConstraints :: Over (CompilerState a) [CompilerConstraint a]
overCompilerConstraints fn CompilerState{..} =
  CompilerState
    { compilerConstraints = fn compilerConstraints
    , ..
    }

{-# INLINE overCompilerAssumptions #-}
overCompilerAssumptions :: Over (CompilerState a) [CompilerAssumption a]
overCompilerAssumptions fn CompilerState{..} =
  CompilerState
    { compilerAssumptions = fn compilerAssumptions
    , ..
    }

{-# INLINE overCompilerStateConstraintsGenErrors #-}
overCompilerStateConstraintsGenErrors :: Over (CompilerState a) [ConstraintsGenError a]
overCompilerStateConstraintsGenErrors fn CompilerState{..} =
  CompilerState
    { compilerConstraintsGenErrors = fn compilerConstraintsGenErrors
    , ..
    }

{-# INLINE overCompilerTypeAnnotationParams #-}
overCompilerTypeAnnotationParams :: Over (CompilerState a) (Dictionary (a, TypeIndex Kind))
overCompilerTypeAnnotationParams fn CompilerState{..} =
  CompilerState
    { compilerTypeAnnotationParams = fn compilerTypeAnnotationParams
    , ..
    }

{-# INLINE overCompilerSolverRuleViolations #-}
overCompilerSolverRuleViolations :: Over (CompilerState a) [InferenceRule Kind a]
overCompilerSolverRuleViolations fn CompilerState{..} =
  CompilerState
    { compilerSolverRuleViolations = fn compilerSolverRuleViolations
    , ..
    }

{-# INLINE overCompilerVerbatimSource #-}
overCompilerVerbatimSource :: Over (CompilerState a) (Environment Text)
overCompilerVerbatimSource fn CompilerState{..} =
  CompilerState
    { compilerVerbatimSource = fn compilerVerbatimSource
    , ..
    }

{-# INLINE overCompilerConfig #-}
overCompilerConfig :: Over (CompilerState a) CompilerConfig
overCompilerConfig fn CompilerState{..} =
  CompilerState
    { compilerConfig = fn compilerConfig
    , ..
    }

{-# INLINE overCompilerModules #-}
overCompilerModules :: Over (CompilerState a) (Environment (ModuleBuild a))
overCompilerModules fn CompilerState{..} =
  CompilerState
    { compilerModules = fn compilerModules
    , ..
    }

initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compilerSupply = 0
    , compilerNameStore = mempty
    , compilerCurrentModule = Path mempty
    , compilerSubstitution = mempty
    , compilerConstraints = []
    , compilerConstraintsGenErrors = []
    , compilerSolverRuleViolations = []
    , compilerAssumptions = []
    , compilerTypeAnnotationParams = mempty
    , compilerVerbatimSource = mempty
    , compilerConfig = defaultConfig
    , compilerModules = mempty
    }
