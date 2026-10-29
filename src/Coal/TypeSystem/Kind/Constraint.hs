{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Kind.Constraint (
  KindConstraint (..),
  KindConstraintError (..),
) where

import Coal.Language.Type.Kind (Kind (..))
import Coal.TypeSystem.Kind.Error (KindError (..))

{- | A kind constraint, tagged with the source location it was generated for.

Constraints are emitted untagged by
'Coal.TypeSystem.Kind.Constraint.Generation', and stamped by
'Coal.Compiler.TypeInference.generateKindConstraints', which generates one
definition at a time. That is what makes a failing constraint attributable to a
source location: the kind-indexed type AST carries no metadata of its own.
-}
data KindConstraint a = KEquality a Kind Kind
  deriving (Show, Eq, Ord, Read, Functor)

{- | A kind error raised while generating constraints, tagged with the source
location it was generated for.

'KindError' carries no metadata of its own, so the location travels alongside it.
-}
data KindConstraintError a = KindConstraintError
  { kindConstraintErrorMetadata :: a
  , kindConstraintErrorKindError :: KindError
  }
  deriving (Show, Eq, Ord, Read)
