{-# LANGUAGE LambdaCase #-}

{- |
Invariant checker for local name canonicalization (pass 2).

Verifies that every locally bound name in the expression tree is unique.

= Checked invariant

Local binders are:

  * @let@-bound variable names (from 'ELet' 'Binding's)
  * Lambda parameter names (from 'ELam')
  * Pattern-bound variable names in @case@ clauses (the non-constructor labels
    in each 'Clause', i.e., every label after the first)

Top-level function names, data constructor names, and type names are not
considered local binders and are excluded from this check.

= Error reporting

Returns one 'DuplicateLocalBinder' entry per unique name that appears more than
once.
-}
module Coal.Kernel.Pipeline.Invariant.LocalNamesUnique (
  checkLocalNamesUnique,
) where

import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))

{- | Verify that every locally bound name in the expression tree is unique.

= Local binders

  * @let@-bound variable names (from 'ELet' 'Binding's)
  * Lambda parameter names (from 'ELam')
  * Pattern-bound variable names in @case@ clauses (the non-constructor labels
    in each 'Clause', i.e., every label after the first)

Top-level function names, data constructor names, and type names are not
considered local binders and are excluded from this check.

Returns an empty list when all local binder names are distinct, or one
'DuplicateLocalBinder' entry per unique name that appears more than once.
-}
checkLocalNamesUnique :: Expr Type -> [InvariantError]
checkLocalNamesUnique expr =
  DuplicateLocalBinder <$> findDuplicates (collectLocalBinders expr)

{- | Recursively collect the names of all locally bound variables in the
expression tree.
-}
collectLocalBinders :: Expr Type -> [Name]
collectLocalBinders =
  \case
    EVar _ ->
      []
    ECon _ ->
      []
    ELit _ ->
      []
    ENil ->
      []
    ELet bindings body ->
      foldMap bindingBinders (NonEmpty.toList bindings)
        <> collectLocalBinders body
    ELam params body ->
      (labelName <$> NonEmpty.toList params)
        <> collectLocalBinders body
    EApp _ f args ->
      collectLocalBinders f
        <> foldMap collectLocalBinders args
    EIf cond t f ->
      collectLocalBinders cond
        <> collectLocalBinders t
        <> collectLocalBinders f
    EOp op ->
      foldMap collectLocalBinders op
    ECase _ scrutinee clauses ->
      collectLocalBinders scrutinee
        <> foldMap clauseBinders (NonEmpty.toList clauses)
    EExt _ e1 e2 ->
      collectLocalBinders e1
        <> collectLocalBinders e2
    EGet _ e ->
      collectLocalBinders e
    ECall _ es e ->
      foldMap collectLocalBinders es
        <> collectLocalBinders e

{- | Collect the binder name introduced by a @let@ binding and then recurse
into the binding's definition expression.
-}
bindingBinders :: Binding Type -> [Name]
bindingBinders (Binding (Label _ name) e) =
  name : collectLocalBinders e

{- | Collect the pattern-bound variable names from a clause (all labels after
the first, which is the constructor) and recurse into the clause body.
-}
clauseBinders :: Clause Type -> [Name]
clauseBinders (Clause labels body) =
  case labels of
    _ :| rest -> (labelName <$> rest) <> collectLocalBinders body

-- | Extract the 'Name' from a 'Label'.
labelName :: Label t -> Name
labelName (Label _ name) = name

{- | Return the unique set of names that appear more than once in the input
list.
-}
findDuplicates :: [Name] -> [Name]
findDuplicates names = nub [a | (a, b) <- zip sorted (drop 1 sorted), a == b]
 where
  sorted = sort names
