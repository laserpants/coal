module Coal.TypeSystem.Kind.Constraint.Solver (solveKindConstraints) where

import Coal.TypeSystem.Kind.Constraint (KindConstraint (..))
import Coal.TypeSystem.Kind.Error (KindError (..))
import Coal.TypeSystem.Kind.Substitution (KindSubstitutable (..), KindSubstitution (..))
import Coal.TypeSystem.Kind.Unification

{- | Solve kind constraints in order, applying each discovered substitution to the
remaining constraints.

On failure the result pairs the 'KindError' with the metadata of the constraint
that could not be unified, so that the caller can report it against the
definition it came from: 'unifyKinds' only knows the two kinds involved, not the
constraint that demanded their equality.
-}
solveKindConstraints :: [KindConstraint a] -> Either (KindError, a) KindSubstitution
solveKindConstraints [] =
  pure mempty
solveKindConstraints (KEquality loc k1 k2 : cs) = do
  sub1 <- case kindUnifierMonad (unifyKinds k1 k2) of
    Left err ->
      Left (err, loc)
    Right sub ->
      Right sub
  sub2 <- solveKindConstraints (applyKinds sub1 cs)
  pure (sub2 <> sub1)
