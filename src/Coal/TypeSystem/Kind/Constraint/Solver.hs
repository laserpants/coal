module Coal.TypeSystem.Kind.Constraint.Solver (protoOsolveKindConstraints) where

import Coal.TypeSystem.Kind.Constraint (KindConstraint (..))
import Coal.TypeSystem.Kind.Substitution (KindSubstitutable (..), KindSubstitution (..))
import Coal.TypeSystem.Kind.Unification

protoOsolveKindConstraints :: [KindConstraint] -> KindUnifier KindSubstitution
protoOsolveKindConstraints [] =
  pure mempty
protoOsolveKindConstraints (KEquality k1 k2 : cs) = do
  sub1 <- unifyKinds k1 k2
  sub2 <- protoOsolveKindConstraints (protoOapplyKinds sub1 cs)
  pure (sub2 <> sub1)
