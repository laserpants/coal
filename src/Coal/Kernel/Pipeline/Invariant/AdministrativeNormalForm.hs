{- |
Invariant checker for administrative normal form (pass 10).

Verifies that all expressions are in administrative normal form (ANF).

= Checked invariants

After pass 10, the program must satisfy:

  1. All operands (application function/arguments, operator inputs, record
     fields, scrutinees, conditions) are atomic (@EVar@, @ECon@, @ELit@,
     @ENil@)
  2. Control flow (@EIf@, @ECase@) may appear in tail position or as a
     @let@-binding RHS; every other position requires atomic operands.
     @EIf@/@ECase@ are never used as a direct operand, because the ANF pass
     binds them to a fresh variable first.

= Error reporting

Returns one error per violation: 'NonAtomicOperand' or
'ControlFlowInNonTailPosition'.
-}
module Coal.Kernel.Pipeline.Invariant.AdministrativeNormalForm (
  checkAdministrativeNormalForm,
) where

import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))

{- | Verify that all expressions are in administrative normal form (ANF).

After pass 10, the program must satisfy:

  1. All operands (application function/arguments, operator inputs, record
     fields, scrutinees, conditions) are atomic (@EVar@, @ECon@, @ELit@,
     @ENil@)
  2. Control flow (@EIf@, @ECase@) may appear in tail position or as a
     @let@-binding RHS; every other position requires atomic operands.

Returns an empty list when the invariant holds, or one error per violation.
-}
checkAdministrativeNormalForm :: Expr Type -> [InvariantError]
checkAdministrativeNormalForm = checkInTailPosition

-- | Check if an expression is atomic ('EVar', 'ECon', 'ELit', or 'ENil').
isAtomic :: Expr Type -> Bool
isAtomic expr =
  case expr of
    EVar _ -> True
    ECon _ -> True
    ELit _ -> True
    ENil -> True
    _ -> False

-- | Check an expression in tail position (control flow is allowed)
checkInTailPosition :: Expr Type -> [InvariantError]
checkInTailPosition expr =
  case expr of
    EVar _ ->
      []
    ECon _ ->
      []
    ELit _ ->
      []
    ENil ->
      []
    ELet bindings body ->
      -- Bindings must not contain control flow, and their RHS must be checked in operand position
      foldMap checkBinding (NonEmpty.toList bindings)
        -- Body is in tail position
        ++ checkInTailPosition body
    ELam _ body ->
      -- Lambda body is in tail position (though lambdas should be lifted)
      checkInTailPosition body
    EApp _ f args ->
      -- Function and all arguments must be atomic
      checkAtomic f
        ++ foldMap checkAtomic args
    EIf cond thenBranch elseBranch ->
      -- Condition must be atomic
      checkAtomic cond
        -- Branches are in tail position
        ++ checkInTailPosition thenBranch
        ++ checkInTailPosition elseBranch
    EOp op ->
      -- All operator operands must be atomic
      foldMap checkAtomic op
    ECase _ scrutinee clauses ->
      -- Scrutinee must be atomic
      checkAtomic scrutinee
        -- All clause bodies are in tail position
        ++ foldMap checkClauseBody (NonEmpty.toList clauses)
    EExt _ fieldValue tailRow ->
      -- Field value and tail row must be atomic
      checkAtomic fieldValue
        ++ checkAtomic tailRow
    EGet _ record ->
      -- Record operand must be atomic
      checkAtomic record
    ECall _ args k ->
      -- All arguments must be atomic; continuation is in tail position
      foldMap checkAtomic args
        ++ checkInTailPosition k

-- | Check that an expression is atomic, report error if not
checkAtomic :: Expr Type -> [InvariantError]
checkAtomic expr =
  if isAtomic expr
    then []
    else case expr of
      -- Control flow in operand position is a specific error
      EIf{} ->
        [ControlFlowInOperandPosition]
      ECase{} ->
        [ControlFlowInOperandPosition]
      -- Any other non-atomic expression in operand position
      _ ->
        [NonAtomicOperand]

-- | Check a let-binding RHS. Control flow is permitted as the RHS of a
-- @let@-binding (the ANF pass binds control flow found in operand position to
-- a fresh variable); the condition/scrutinee must be atomic and the
-- branches/clause bodies checked recursively.
checkBinding :: Binding Type -> [InvariantError]
checkBinding (Binding _ expr) = case expr of
  EIf cond thenBranch elseBranch ->
    checkAtomic cond
      ++ checkInTailPosition thenBranch
      ++ checkInTailPosition elseBranch
  ECase _ scrutinee clauses ->
    checkAtomic scrutinee
      ++ foldMap checkClauseBody (NonEmpty.toList clauses)
  -- For all other expressions, check them in operand context
  -- (they must not contain control flow in their subexpressions)
  _ ->
    checkInOperandPosition expr

-- | Check an expression in operand position (control flow is forbidden)
checkInOperandPosition :: Expr Type -> [InvariantError]
checkInOperandPosition expr =
  case expr of
    EVar _ ->
      []
    ECon _ ->
      []
    ELit _ ->
      []
    ENil ->
      []
    ELet bindings body ->
      -- Bindings checked as usual
      foldMap checkBinding (NonEmpty.toList bindings)
        -- Body is still in operand position (not tail)
        ++ checkInOperandPosition body
    ELam _ body ->
      -- Lambda body in operand position (though lambdas should be lifted)
      checkInOperandPosition body
    EApp _ f args ->
      -- Function and all arguments must be atomic
      checkAtomic f
        ++ foldMap checkAtomic args
    EIf{} ->
      -- Control flow in operand position
      [ControlFlowInOperandPosition]
    EOp op ->
      -- All operator operands must be atomic
      foldMap checkAtomic op
    ECase{} ->
      -- Control flow in operand position
      [ControlFlowInOperandPosition]
    EExt _ fieldValue tailRow ->
      -- Field value and tail row must be atomic
      checkAtomic fieldValue
        ++ checkAtomic tailRow
    EGet _ record ->
      -- Record operand must be atomic
      checkAtomic record
    ECall _ args k ->
      -- All arguments must be atomic; continuation is in operand position
      foldMap checkAtomic args
        ++ checkInOperandPosition k

-- | Check a clause body (in tail position)
checkClauseBody :: Clause Type -> [InvariantError]
checkClauseBody (Clause _ body) = checkInTailPosition body
