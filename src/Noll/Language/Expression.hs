{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression (
  Expression (..),
  Clause (..),
  CompiledClause (..),
) where

import Data.Data (Data, Typeable)
import Noll.Common.List1 (List1)
import Noll.Label (Label (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..))
import Noll.Language.Expression.Operator.Binary (BinaryOperator)
import Noll.Language.Expression.Operator.Unary (UnaryOperator)
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Parameter (..), Type)
import Noll.Utils (Dictionary, Name)

data Clause a t = EClause a (Pattern a t) (List1 (Choice Expression a t))
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

data CompiledClause a t
  = ECompiledClause (List1 (Label t)) (Expression a t)
  | ECompiledField Name (Label t) (Label t) (Expression a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

data Expression a t
  = -- | Type-annotated expression
    EAnnotation a (Type Parameter ()) (Expression a t)
  | -- | Function application
    EApplication a t (Expression a t) (List1 (Expression a t))
  | -- | Lambda function expression
    ELambda a (List1 (Pattern a t)) (Expression a t)
  | -- | Let-binding
    ELet a (List1 (Binding Expression a t)) (Expression a t)
  | -- | Recursive let-binding
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
    EUnaryOperator a t UnaryOperator
  | -- | Binary operator
    EBinaryOperator a t BinaryOperator
  | -- | Record
    ERecord a t (Dictionary (Expression a t)) (Maybe (Expression a t))
  | -- | List cons-operator
    EListCons a t (Expression a t) (Expression a t)
  | -- | List literal
    EListLiteral a t [Expression a t]
  | -- | Tuples
    ETuple a t (List1 (Expression a t))
  | -- | Pattern matching expression
    EMatch a t (Expression a t) (List1 (Clause a t))
  | -- | Compiled match expression
    ECompiledMatch a t (Expression a t) (List1 (CompiledClause a t))
  | -- | Fold expression
    EFold a t (List1 (Expression a t)) (List1 (Clause a t)) (Maybe (Expression a t))
  | -- | Record field access selector
    ESelect a (Label t) (Expression a t)
  | -- | TODO
    EDictionaryLambda a (List1 (Trait t)) (Expression a t)
  | -- | TODO
    EDictionaryApplication a t (Expression a t) (List1 (Trait t)) [Expression a t]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
