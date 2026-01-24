{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Extras (Dictionary, Name)
import GHC.Generics (Generic)

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
    ELambdaMatch a t (NonEmpty (Clause a t))
  | -- | Compiled match expression
    ECompiledMatch a t (Expression a t) (NonEmpty (CompiledClause a t))
  | -- | Fold expression
    EFold a t (NonEmpty (Expression a t)) (NonEmpty (Clause a t))
  | -- | Record field selector
    ESelect a (Label t) (Expression a t)
  | -- | Row restriction
    EFocus a Name (Label t) (Label t) (Expression a t) (Expression a t)
  | -- | Trait instance dictionary
    ETraitInstance a t (Trait t)
  | -- | FFI function call
    EFFICall a t (Label (Type Parameter ())) [Expression a t] (Expression a t)
  | -- | Do-notation block
    EDoBlock a (NonEmpty (Pattern a t, Expression a t))
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary t) => Binary (Expression a t)

data Clause a t = EClause a (Pattern a t) (NonEmpty (Choice Expression a t))
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary t) => Binary (Clause a t)

data CompiledClause a t = ECompiledClause a (NonEmpty (Label t)) (Expression a t)
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary t) => Binary (CompiledClause a t)
