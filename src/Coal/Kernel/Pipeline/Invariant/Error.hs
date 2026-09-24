{- |
Invariant violation errors.

Defines the error type for pipeline invariant checkers. Each constructor
represents a specific violation of a compiler-pass invariant.

Invariant checkers are pure functions that traverse expressions and modules to
verify that a particular normalization pass has established its claimed
invariant. They are used for testing and debugging the pipeline.
-}
module Coal.Kernel.Pipeline.Invariant.Error (
  InvariantError (..),
) where

import Coal.Common.Name (Name)

{- | A violation of a compiler-pass invariant discovered by one of the pure
checker functions in @Coal.Kernel.Pipeline.Invariant.*@.
-}
data InvariantError
  = {- | A local binder name appears more than once in the same expression
    tree (across all binding forms: @let@, lambda parameters, and
    constructor pattern variables).
    -}
    DuplicateLocalBinder Name
  | {- | A lambda expression directly returns another lambda expression,
    i.e. the body of a @fn@ is itself a @fn@.
    -}
    NestedLambdaBody
  | {- | A top-level constant directly contains a lambda expression and should
    have been promoted to a function object by pass 005.
    -}
    ConstantContainsLambda Name
  | {- | A top-level function's body is directly a lambda expression and should
    have been absorbed into the function's parameter list by pass 005.
    -}
    FunctionBodyIsLambda Name
  | {- | A logical AND operator (@&&@) node is still present in the expression
    tree; it should have been translated to an @if-then-else@ by pass 007.
    -}
    AndOperatorPresent
  | {- | A logical OR operator (@||@) node is still present in the expression
    tree; it should have been translated to an @if-then-else@ by pass 007.
    -}
    OrOperatorPresent
  | {- | A lambda expression node (@ELam@) is still present in the expression
    tree; it should have been lifted to a top-level function by pass 004.
    -}
    LambdaNotLifted
  | {- | A top-level function has a result type that is itself a function type;
    it should have been saturated with additional parameters by pass 006.
    -}
    FunctionResultIsFunction Name
  | {- | A data constructor is not fully saturated (applied to fewer arguments
    than its arity); it should have been wrapped in a lambda by pass 003.
    -}
    UnsaturatedConstructor Name
  | {- | A trivial alias binding (@x = y@) remains in a let-expression;
    it should have been eliminated by pass 008.
    -}
    TrivialAliasBinding Name
  | {- | A control-flow expression (@EIf@ or @ECase@) appears in a non-tail
    position (operand position); it should have been lifted by pass 009.
    -}
    ControlFlowInOperandPosition
  | {- | A non-atomic expression appears where only atomic expressions are
    allowed; it should have been normalized by pass 009.
    -}
    NonAtomicOperand
  deriving (Show, Eq, Ord)
