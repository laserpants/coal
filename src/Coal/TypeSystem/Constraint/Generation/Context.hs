{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.Context (
  ConstraintsGenContext (..),
  overConstraintsGenMonomorphicSet,
  emptyConstraintsGenContext,
) where

import Coal.Compiler.Build (ModuleBuild, emptyModuleBuild)
import Coal.TypeSystem.Constraint (Monomorphic (..))

data ConstraintsGenContext g o a t = ConstraintsGenContext
  { constraintsGenContextMonomorphicSet :: Monomorphic (o a)
  , constraintsGenContextModules :: ModuleBuild g
  }
  deriving (Show, Eq, Ord)

emptyConstraintsGenContext :: (Ord (o a)) => ConstraintsGenContext g o a t
emptyConstraintsGenContext =
  ConstraintsGenContext
    { constraintsGenContextMonomorphicSet = mempty
    , constraintsGenContextModules = emptyModuleBuild
    }

{-# INLINE overConstraintsGenMonomorphicSet #-}
overConstraintsGenMonomorphicSet :: (Monomorphic (o a) -> Monomorphic (o a)) -> ConstraintsGenContext g o a t -> ConstraintsGenContext g o a t
overConstraintsGenMonomorphicSet fn ConstraintsGenContext{..} =
  ConstraintsGenContext
    { constraintsGenContextMonomorphicSet = fn constraintsGenContextMonomorphicSet
    , ..
    }
