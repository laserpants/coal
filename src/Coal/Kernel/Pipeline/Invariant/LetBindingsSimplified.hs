{- |
Invariant checker for let-binding simplification (pass 9).

Verifies that no trivial alias bindings remain in let-expressions.

= Checked invariant

After pass 9 (let-binding simplification and alias elimination), all trivial
alias bindings of the form @x = y@ (where the right-hand side is a variable)
should have been eliminated.

This check ensures that for every let-binding @Binding name expr@, the
expression @expr@ is not simply @EVar _@.

= Error reporting

Returns one 'TrivialLetBinding' error per violation.
-}
module Coal.Kernel.Pipeline.Invariant.LetBindingsSimplified (
  checkLetBindingsSimplified,
) where

import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))

{- | Verify that no trivial alias bindings remain in let-expressions.

After pass 9 (let-binding simplification and alias elimination), all trivial
alias bindings of the form @x = y@ (where the right-hand side is a variable)
should have been eliminated.

This check ensures that for every let-binding @Binding name expr@, the
expression @expr@ is not simply @EVar _@.

Returns an empty list when the invariant holds, or one error per violation.
-}
checkLetBindingsSimplified :: Expr Type -> [InvariantError]
checkLetBindingsSimplified expr = case expr of
  EVar _ ->
    []
  ECon _ ->
    []
  ELit _ ->
    []
  ENil ->
    []
  ELet bindings body ->
    -- Check each binding for trivial aliases
    foldMap checkBindingNotTrivial (NonEmpty.toList bindings)
      -- Also recurse into the binding definitions and body
      ++ foldMap checkBindingExpr (NonEmpty.toList bindings)
      ++ checkLetBindingsSimplified body
  ELam _ body ->
    checkLetBindingsSimplified body
  EApp _ f args ->
    checkLetBindingsSimplified f
      ++ foldMap checkLetBindingsSimplified args
  EIf cond thenBranch elseBranch ->
    checkLetBindingsSimplified cond
      ++ checkLetBindingsSimplified thenBranch
      ++ checkLetBindingsSimplified elseBranch
  EOp op ->
    foldMap checkLetBindingsSimplified op
  ECase _ scrutinee clauses ->
    checkLetBindingsSimplified scrutinee
      ++ foldMap checkClauseBody (NonEmpty.toList clauses)
  EExt _ e1 e2 ->
    checkLetBindingsSimplified e1
      ++ checkLetBindingsSimplified e2
  EGet _ e ->
    checkLetBindingsSimplified e
  ECall _ args k ->
    foldMap checkLetBindingsSimplified args
      ++ checkLetBindingsSimplified k

-- | Check if a binding is a trivial alias (x = y) and report it as an error.
checkBindingNotTrivial :: Binding Type -> [InvariantError]
checkBindingNotTrivial (Binding (Label _ name) expr) = case expr of
  EVar _ ->
    -- This is a trivial alias binding - it should have been eliminated
    [TrivialAliasBinding name]
  _ ->
    -- Not a trivial alias
    []

-- | Recurse into a binding's definition expression.
checkBindingExpr :: Binding Type -> [InvariantError]
checkBindingExpr (Binding _ e) = checkLetBindingsSimplified e

-- | Recurse into a clause's body expression.
checkClauseBody :: Clause Type -> [InvariantError]
checkClauseBody (Clause _ body) = checkLetBindingsSimplified body
