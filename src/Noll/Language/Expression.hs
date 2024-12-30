{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression (Expression (..), Clause (..)) where

import Noll.Label (Label (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..))
import Noll.Language.Expression.Operator.Binary (BinaryOperator)
import Noll.Language.Expression.Operator.Unary (UnaryOperator)
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Language.Type (Type, TypeId (..))
import Noll.Utils (Dictionary, Some)

data Clause e a t = EClause a (Pattern a t) (Some (Choice e a t))
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data Expression a t
  = -- | Type-annotated expression
    EAnnotation (Type TypeId ()) (Expression a t)
  | -- | Function application
    EApplication a t (Expression a t) (Some (Expression a t))
  | -- | Lambda function expression
    ELambda a (Some (Pattern a t)) (Expression a t)
  | -- | Let binding
    ELet a (Some (Binding Expression a t)) (Expression a t)
  | -- | Recursive let binding
    ERecursiveLet a (Pattern a t) (Expression a t) (Expression a t)
  | -- | Variable
    EVariable a (Label t)
  | -- | Data constructor
    EConstructor a (Label t)
  | -- | Literal expression
    ELiteral a Primitive
  | -- | If-else statement
    EIf a t (Expression a t) (Expression a t) (Expression a t)
  | -- | Unary operator
    EUnaryOperator a (t, UnaryOperator)
  | -- | Binary operators
    EBinaryOperator a (t, BinaryOperator)
  | -- | Record
    ERecord a t (Dictionary (Expression a t)) (Maybe (Expression a t))
  | -- | List cons-operator
    EListCons a t (Expression a t) (Expression a t)
  | -- | List literal
    EListLiteral a t [Expression a t]
  | -- | Pattern matching expression
    EMatch a t (Expression a t) (Some (Clause Expression a t))
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
