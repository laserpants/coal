{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.Context (
  ConstraintsGenContext (..),
  overConstraintsGenMonomorphicSet,
  emptyConstraintsGenContext,
) where

import Coal.Common.Environment (Environment (..))
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.TypeSystem.Constraint (Monomorphic (..))

data ConstraintsGenContext g o a t = ConstraintsGenContext
  { constraintsGenContextMonomorphicSet :: Monomorphic (o a)
  , constraintsGenContextDataConstructors :: Environment (ProtoDataConstructorEntry g)
  , constraintsGenContextTypeConstructors :: Environment (ProtoTypeConstructorEntry g)
  }
  deriving (Show, Eq, Ord)

emptyConstraintsGenContext :: (Ord (o a)) => ConstraintsGenContext g o a t
emptyConstraintsGenContext =
  ConstraintsGenContext
    { constraintsGenContextMonomorphicSet = mempty
    , constraintsGenContextDataConstructors = mempty
    , constraintsGenContextTypeConstructors = mempty
    }

{-# INLINE overConstraintsGenMonomorphicSet #-}
overConstraintsGenMonomorphicSet :: (Monomorphic (o a) -> Monomorphic (o a)) -> ConstraintsGenContext g o a t -> ConstraintsGenContext g o a t
overConstraintsGenMonomorphicSet fn ConstraintsGenContext{..} =
  ConstraintsGenContext
    { constraintsGenContextMonomorphicSet = fn constraintsGenContextMonomorphicSet
    , ..
    }
