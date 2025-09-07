{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Expression (
  Expression (..),
  Clause (..),
  CompiledClause (..),
) where

import Coal.Common.Label (Label (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..))
import Coal.Language.Expression.Operator (BinaryOperator, UnaryOperator)
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Primitive (Primitive (..))
import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter (..), Type)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Extra (Dictionary, Name)

data Expression a t
  = -- | Type-annotated expression
    EAnnotation a (Type Parameter ()) (Expression a t)
  | -- | Function application
    EApplication a t (Expression a t) (NonEmpty (Expression a t))
  | -- | Lambda function expression
    ELambda a (NonEmpty (Pattern a t)) (Expression a t)
  | -- | Let-binding
    ELet a (NonEmpty (Binding Expression a t)) (Expression a t)
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
    ETuple a t (NonEmpty (Expression a t))
  | -- | Pattern matching expression
    EMatch a t (Expression a t) (NonEmpty (Clause a t))
  | -- | Lambda-style match expression
    ELambdaMatch a t (NonEmpty (Clause a t)) (Maybe (Expression a t))
  | -- | Compiled match expression
    ECompiledMatch a t (Expression a t) (NonEmpty (CompiledClause a t))
  | -- | Fold expression
    EFold a t (NonEmpty (Expression a t)) (NonEmpty (Clause a t)) (Maybe (Expression a t))
  | -- | Record field selector
    ESelect a (Label t) (Expression a t)
  | -- | Codata field selector
    ECodataSelect a (Label t) (Expression a t) (Maybe (Expression a t))
  | -- | Codata internals
    ECodataFields a t (Dictionary (Expression a t))
  | -- | Row restriction
    EFocus Name (Label t) (Label t) (Expression a t) (Expression a t)
  | -- | Trait instance dictionary
    ETraitDictionary a t (Trait t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

data Clause a t = EClause a (Pattern a t) (NonEmpty (Choice Expression a t))
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

data CompiledClause a t = ECompiledClause (NonEmpty (Label t)) (Expression a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)
