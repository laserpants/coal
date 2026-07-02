{- |
Type error definitions.

Defines the error types used by the type checker, including:

  * Error contexts (module, object, expression)
  * Error kinds (type mismatch, arity mismatch, undefined names, etc.)
  * Structured error records combining context and kind

All type errors are accumulated during checking via a 'Writer' monad, allowing
the checker to continue after encountering problems.
-}
module Coal.Kernel.TypeCheck.Error (
  TypeError (..),
  TypeErrorKind (..),
  Context (..),
) where

import Coal.Kernel.Language.Type (Type)
import Common (Name)

data Context
  = InModule Name
  | InObject Name
  | InExpression
  deriving (Show, Eq)

data TypeErrorKind
  = TypeMismatch Type Type
  | VariableNotFound Name
  | ConstructorNotFound Name
  | ArityMismatch Int Int
  | FieldNotFound Name Type
  | NotAFunction Type
  | ConditionNotBool Type
  | BranchTypeMismatch Type Type
  deriving (Show, Eq)

data TypeError = TypeError
  { errorContext :: Context
  , errorKind :: TypeErrorKind
  }
  deriving (Show, Eq)
