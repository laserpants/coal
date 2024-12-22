{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression (Expression (..)) where

import Noll.Label (Label (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Operator.Binary (BinaryOperator)
import Noll.Language.Expression.Operator.Unary (UnaryOperator)
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Utils (Some)

data Expression t
  = -- | Function application
    Application t (Expression t) (Some (Expression t))
  | -- | Lambda function expression
    Lambda (Some (Pattern t)) (Expression t)
  | -- | Let binding
    Let (Some (Binding Expression t)) (Expression t)
  | -- | Variable
    Variable (Label t)
  | -- | Data constructor
    Constructor (Label t)
  | -- | Literal expression
    Literal Primitive
  | -- | If-else statement
    If (Expression t) (Expression t) (Expression t)
  | -- | Unary operator
    UnaryOperator (t, UnaryOperator)
  | -- | Binary operators
    BinaryOperator (t, BinaryOperator)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
