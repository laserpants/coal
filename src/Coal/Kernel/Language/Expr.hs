{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

{- |
Core expression language.

Defines the abstract syntax tree for Coal kernel language expressions, including
variables, constructors, literals, lambdas, let-bindings, conditionals, case
expressions, operators, and function applications.

The expression type is parameterized over a type annotation @t@, allowing the
same AST structure to be used at different compilation stages (e.g., with
'Name' for untyped expressions or 'Type' for type-checked expressions).
-}
module Coal.Kernel.Language.Expr (
  Label (..),
  Binding (..),
  Clause (..),
  Expr (..),
) where

import Data.List.NonEmpty (NonEmpty)

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Op (Op)
import Coal.Kernel.Language.Prim (Prim)

data Label t = Label t Name
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data Binding t = Binding (Label t) (Expr t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data Clause t = Clause (NonEmpty (Label t)) (Expr t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

{- | Core language expression grammar.

The expression grammar supports:

  * Variables and constructors
  * Function abstraction (@fn@) and application
  * Let-bindings (non-recursive)
  * Conditional expressions (@if@)
  * Pattern matching (@case@)
  * Primitive literals and operators
  * Record construction, extension and projections
  * External C function calls
-}
data Expr t
  = -- | Variable
    EVar (Label t)
  | -- | Constructor
    ECon (Label t)
  | -- | Let-binding
    ELet (NonEmpty (Binding t)) (Expr t)
  | -- | Literal primitive value
    ELit Prim
  | -- | Lambda abstraction
    ELam (NonEmpty (Label t)) (Expr t)
  | -- | Function application
    EApp t (Expr t) (NonEmpty (Expr t))
  | -- | If-statement
    EIf (Expr t) (Expr t) (Expr t)
  | -- | Operators
    EOp (Op (Expr t))
  | -- | Pattern matching expression
    ECase t (Expr t) (NonEmpty (Clause t))
  | -- | Record field extension
    EExt Name (Expr t) (Expr t)
  | -- | Empty record
    ENil
  | -- | Field projection
    EGet (Label t) (Expr t)
  | -- | External C function call
    ECall (Label t) [Expr t] (Expr t)
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    )
