{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression (Expression (..)) where

import Noll.Label (Label (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Operator.Binary (BinaryOperator)
import Noll.Language.Expression.Operator.Unary (UnaryOperator)
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Utils (Dictionary, Some)

data Expression t
  = -- | Function application
    EApplication t (Expression t) (Some (Expression t))
  | -- | Lambda function expression
    ELambda (Some (Pattern t)) (Expression t)
  | -- | Let binding
    ELet (Some (Binding Expression t)) (Expression t)
  | -- | Recursive let binding
    ERecursiveLet (Pattern t) (Expression t) (Expression t)
  | -- | Variable
    EVariable (Label t)
  | -- | Data constructor
    EConstructor (Label t)
  | -- | Literal expression
    ELiteral Primitive
  | -- | If-else statement
    EIf (Expression t) (Expression t) (Expression t)
  | -- | Unary operator
    EUnaryOperator (t, UnaryOperator)
  | -- | Binary operators
    EBinaryOperator (t, BinaryOperator)
  | -- | Record
    ERecord t (Dictionary (Expression t)) (Maybe (Expression t))
  | -- | List cons-operator
    EListCons t (Expression t) (Expression t)
  | -- | List literal
    EListLiteral t [Expression t]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
