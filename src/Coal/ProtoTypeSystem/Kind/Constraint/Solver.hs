module Coal.ProtoTypeSystem.Kind.Constraint.Solver (protoOsolveKindConstraints) where

import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Coal.ProtoTypeSystem.Kind.Substitution (ProtoKindSubstitutable (..), ProtoKindSubstitution (..))
import Coal.ProtoTypeSystem.Kind.Unification

protoOsolveKindConstraints :: [ProtoKindConstraint] -> ProtoKindUnifier ProtoKindSubstitution
protoOsolveKindConstraints [] =
  pure mempty
protoOsolveKindConstraints (ProtoKEquality k1 k2 : cs) = do
  sub1 <- unifyKinds k1 k2
  sub2 <- protoOsolveKindConstraints (protoOapplyKinds sub1 cs)
  pure (sub2 <> sub1)
