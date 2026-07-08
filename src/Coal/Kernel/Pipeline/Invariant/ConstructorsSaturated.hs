{-# LANGUAGE LambdaCase #-}

{- |
Invariant checker for constructor saturation (pass 4).

Verifies that every data constructor application is fully saturated, i.e.,
applied to exactly its declared arity.

= Checked invariant

  1. No standalone constructor with @arity > 0@ exists (e.g., bare @Just@ or
     @Pair@).
  2. Every constructor application @C(e₁, …, eₖ)@ satisfies @k = arity(C)@.

Partially applied constructors must be wrapped in lambda expressions.

= Error reporting

Returns one error per violation: 'UnsaturatedConstructor' for standalone
constructors with positive arity, or 'OversaturatedConstructor' for
applications with too many arguments.
-}
module Coal.Kernel.Pipeline.Invariant.ConstructorsSaturated (
  checkConstructorsSaturated,
) where

import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.Function (arity)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))

{- | Verify that every data constructor application is fully saturated.

After pass 4 (constructor saturation), all constructors must be applied to
exactly their declared arity. Partially applied constructors must be wrapped in
lambda expressions.

= Checked properties

  1. No standalone constructor with @arity > 0@ exists (e.g., bare @Just@ or
     @Pair@).
  2. Every constructor application @C(e₁, …, eₖ)@ satisfies @k = arity(C)@.

Returns an empty list when the invariant holds, or one error per violation.
-}
checkConstructorsSaturated :: Expr Type -> [InvariantError]
checkConstructorsSaturated =
  \case
    EVar{} ->
      []
    ECon (Label t name) ->
      -- Standalone constructor must have arity 0 (like True, False, Nil)
      ([UnsaturatedConstructor name | arity t > 0])
    ELit{} ->
      []
    ENil{} ->
      []
    ELet bindings body ->
      foldMap checkBinding (NonEmpty.toList bindings)
        ++ checkConstructorsSaturated body
    ELam _ body ->
      checkConstructorsSaturated body
    EApp _ f args ->
      case f of
        ECon (Label t name) ->
          -- Constructor application - verify full saturation
          let expectedArity = arity t
              actualArity = NonEmpty.length args
           in ([UnsaturatedConstructor name | actualArity /= expectedArity])
        _ ->
          -- Not a constructor application - recurse into function
          checkConstructorsSaturated f
        ++ foldMap checkConstructorsSaturated args
    EIf cond thenBranch elseBranch ->
      checkConstructorsSaturated cond
        ++ checkConstructorsSaturated thenBranch
        ++ checkConstructorsSaturated elseBranch
    EOp op ->
      foldMap checkConstructorsSaturated op
    ECase _ scrutinee clauses ->
      checkConstructorsSaturated scrutinee
        ++ foldMap checkClauseBody (NonEmpty.toList clauses)
    EExt _ e1 e2 ->
      checkConstructorsSaturated e1
        ++ checkConstructorsSaturated e2
    EGet _ e ->
      checkConstructorsSaturated e
    ECall _ args k ->
      foldMap checkConstructorsSaturated args
        ++ checkConstructorsSaturated k

-- | Recurse into a let-binding's definition expression.
checkBinding :: Binding Type -> [InvariantError]
checkBinding (Binding _ e) = checkConstructorsSaturated e

-- | Recurse into a clause's body expression.
checkClauseBody :: Clause Type -> [InvariantError]
checkClauseBody (Clause _ body) = checkConstructorsSaturated body
