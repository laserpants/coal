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
import Coal.Language.Expression.Operator (Operator)
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Primitive (Primitive (..))
import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter (..), Type)
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Extras (Dictionary, Name)
import GHC.Generics (Generic)

data Expression a s t
  = -- | Type-annotated expression
    EAnnotation a (Type Parameter s) (Expression a s t)
  | -- | Function application
    EApplication a t (Expression a s t) (NonEmpty (Expression a s t))
  | -- | Lambda function expression
    ELambda a (NonEmpty (Pattern a s t)) (Expression a s t)
  | -- | Let-binding
    ELet a (NonEmpty (Binding Expression a s t)) (Expression a s t)
  | -- | Recursive let-binding
    ERecursiveLet a (Pattern a s t) (Expression a s t) (Expression a s t)
  | -- | Variable
    EVariable a (Label t)
  | -- | Data constructor
    EConstructor a (Label t)
  | -- | Literal expression
    ELiteral a Primitive
  | -- | If-else statement
    EIf a t (Expression a s t) (Expression a s t) (Expression a s t)
  | -- | Operator
    EOperator a t Operator
  | -- | Record
    ERecord a t (Dictionary (Expression a s t)) (Maybe (Expression a s t))
  | -- | List cons-operator
    EListCons a t (Expression a s t) (Expression a s t)
  | -- | List literal
    EListLiteral a t [Expression a s t]
  | -- | Tuples
    ETuple a t (NonEmpty (Expression a s t))
  | -- | Pattern matching expression
    EMatch a t (Expression a s t) (NonEmpty (Clause a s t))
  | -- | Lambda-style match expression
    ELambdaMatch a t (NonEmpty (Clause a s t))
  | -- | Compiled match expression
    ECompiledMatch a t (Expression a s t) (NonEmpty (CompiledClause a s t))
  | -- | Fold expression
    EFold a t (NonEmpty (Expression a s t)) (NonEmpty (Clause a s t))
  | -- | Record field selector
    ESelect a (Label t) (Expression a s t)
  | -- | Row restriction
    EFocus a Name (Label t) (Label t) (Expression a s t) (Expression a s t)
  | -- | Trait instance dictionary
    ETraitInstance a t (Trait t)
  | -- | FFI function call
    EFFICall a t (Label (Type Parameter ())) [Expression a s t] (Expression a s t)
  | -- | Do-notation block
    EDoBlock a (NonEmpty (Pattern a s t, Expression a s t))
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

instance (Binary a, Binary s, Binary t) => Binary (Expression a s t)

data Clause a s t = EClause a (Pattern a s t) (NonEmpty (Choice Expression a s t))
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

instance (Binary a, Binary s, Binary t) => Binary (Clause a s t)

data CompiledClause a s t = ECompiledClause a (NonEmpty (Label t)) (Expression a s t)
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

instance (Binary a, Binary s, Binary t) => Binary (CompiledClause a s t)
