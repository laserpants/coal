module Coal.TypeSystem.Kind.Constraint.Solver (solveKindConstraints) where

import Coal.TypeSystem.Kind.Constraint (KindConstraint (..))
import Coal.TypeSystem.Kind.Substitution (KindSubstitutable (..), KindSubstitution (..))
import Coal.TypeSystem.Kind.Unification

solveKindConstraints :: [KindConstraint] -> KindUnifier KindSubstitution
solveKindConstraints [] =
  pure mempty
solveKindConstraints (KEquality k1 k2 : cs) = do
  sub1 <- unifyKinds k1 k2
  sub2 <- solveKindConstraints (applyKinds sub1 cs)
  pure (sub2 <> sub1)
