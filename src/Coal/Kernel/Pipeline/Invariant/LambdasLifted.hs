{- |
Invariant checker for lambda lifting (pass 5).

Verifies that no lambda expression (@ELam@) nodes remain anywhere in the
expression tree.

= Checked invariant

After lambda lifting, all lambda expressions should have been converted to
top-level functions. Any remaining @ELam@ node indicates that the lifting
transformation was incomplete.

= Error reporting

Returns one 'LambdaNotLifted' entry for each @ELam@ node found, together with
errors from nested subexpressions.
-}
module Coal.Kernel.Pipeline.Invariant.LambdasLifted (
  checkLambdasLifted,
) where

import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))

{- | Verify that no lambda expression (@ELam@) nodes remain anywhere in the
expression tree.

After pass 5 (lambda lifting), all lambda expressions should have been
converted to top-level functions. Any remaining @ELam@ node indicates that the
lifting transformation was incomplete.

Returns an empty list when the invariant holds everywhere, or one
'LambdaNotLifted' entry for each @ELam@ node found, together with errors from
nested subexpressions.
-}
checkLambdasLifted :: Expr Type -> [InvariantError]
checkLambdasLifted expr = case expr of
  EVar _ ->
    []
  ECon _ ->
    []
  ELit _ ->
    []
  ENil ->
    []
  ELet bindings body ->
    foldMap checkBinding (NonEmpty.toList bindings)
      ++ checkLambdasLifted body
  -- Lambda: this should not exist after lambda lifting
  ELam _ _ ->
    [LambdaNotLifted]
  EApp _ f args ->
    checkLambdasLifted f
      ++ foldMap checkLambdasLifted args
  EIf cond t f ->
    checkLambdasLifted cond
      ++ checkLambdasLifted t
      ++ checkLambdasLifted f
  EOp op ->
    foldMap checkLambdasLifted op
  ECase _ scrutinee clauses ->
    checkLambdasLifted scrutinee
      ++ foldMap checkClauseBody (NonEmpty.toList clauses)
  EExt _ e1 e2 ->
    checkLambdasLifted e1
      ++ checkLambdasLifted e2
  EGet _ e ->
    checkLambdasLifted e
  ECall _ args k ->
    foldMap checkLambdasLifted args
      ++ checkLambdasLifted k

-- | Recurse into a let-binding's definition expression.
checkBinding :: Binding Type -> [InvariantError]
checkBinding (Binding _ e) = checkLambdasLifted e

-- | Recurse into a clause's body expression.
checkClauseBody :: Clause Type -> [InvariantError]
checkClauseBody (Clause _ body) = checkLambdasLifted body
