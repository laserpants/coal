{-# LANGUAGE LambdaCase #-}

{- |
Free variable analysis.

Computes the set of free variables in expressions. A variable is /free/ if it
occurs in the expression but is not bound by an enclosing lambda, let-binding,
or case pattern.

Free variable analysis is used during:

  * Lambda lifting to determine closure capture sets
  * Optimization to detect dead code
  * Code generation to build environment records
-}
module Coal.Kernel.FreeVars (
  freeVars,
) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))

{- | Compute the set of /free/ variables occurring in an expression.

A variable occurrence is considered free if:

  * it appears in the expression,
  * it is not introduced by an enclosing binder ('ELam', 'ELet', 'ECase'),
  * and it is not a constructor name ('ECon' or clause constructor).

The result contains complete labels, including type annotations.

= Example

@
fn(x : int32) =>
  add(x, y : int32)
@

has free variables @{ y : int32 }@, not just the name @\"y\"@.
-}
freeVars :: (Ord t) => Expr t -> Set (Label t)
freeVars =
  \case
    -- Variable: the variable itself is free
    EVar x -> Set.singleton x
    -- Constructor: not a variable, no free variables
    ECon _ -> Set.empty
    -- Literal: no free variables
    ELit _ -> Set.empty
    -- Empty record: no free variables
    ENil -> Set.empty
    -- Let: collect free variables from all bindings and body, then remove bound names
    ELet bindings body ->
      let boundNameSet = Set.fromList [n | Label _ n <- bindingName <$> NonEmpty.toList bindings]
          bindingFreeVars = foldMap (freeVars . bindingExpr) bindings
          bodyFreeVars = freeVars body
       in Set.filter
            (\(Label _ n) -> Set.notMember n boundNameSet)
            (bindingFreeVars `Set.union` bodyFreeVars)
    -- Lambda: free variables in body minus the parameters
    ELam params body ->
      let paramNameSet = Set.fromList [n | Label _ n <- NonEmpty.toList params]
       in Set.filter (\(Label _ n) -> Set.notMember n paramNameSet) (freeVars body)
    -- Application: union of free variables in function and all arguments
    EApp _ f args ->
      freeVars f `Set.union` foldMap freeVars args
    -- If-then-else: union of free variables in all three branches
    EIf cond t f ->
      freeVars cond `Set.union` freeVars t `Set.union` freeVars f
    -- Operator: union of free variables in all operands
    EOp op ->
      foldMap freeVars op
    -- Case: free variables in scrutinee and all clauses
    ECase _ scrutinee clauses ->
      freeVars scrutinee `Set.union` foldMap clauseFreeVars clauses
    -- Record extension: union of free variables in both expressions
    EExt _ e1 e2 ->
      freeVars e1 `Set.union` freeVars e2
    -- Record projection: free variables in the record expression
    EGet _ e ->
      freeVars e
    -- External C call: free variables in arguments and continuation
    ECall _ args k ->
      foldMap freeVars args `Set.union` freeVars k

-- | Extract the label (name) from a let binding.
bindingName :: Binding t -> Label t
bindingName (Binding name _) = name

-- | Extract the expression from a let binding.
bindingExpr :: Binding t -> Expr t
bindingExpr (Binding _ e) = e

{- | Compute free variables in a case clause.

The constructor (first label in the NonEmpty list) is not a bound variable.
Only the remaining labels bind pattern variables.
-}
clauseFreeVars :: (Ord t) => Clause t -> Set (Label t)
clauseFreeVars (Clause labels body) =
  Set.filter (\(Label _ n) -> Set.notMember n patternNameSet) (freeVars body)
 where
  patternNameSet = Set.fromList [n | Label _ n <- NonEmpty.tail labels]
