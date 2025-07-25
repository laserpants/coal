{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression (
  Expression (..),
  Clause (..),
  CompiledClause (..),
) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Extra (Dictionary, Name)
import Noll.Common.FreeVars (BoundVars (..), FreeVars (..), exceptNames)
import Noll.Common.Label (Label (..))
import Noll.Common.List1 (List1, NonEmpty ((:|)))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..))
import Noll.Language.Expression.Operator (BinaryOperator, UnaryOperator)
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Parameter (..), Type)

import qualified Data.Set as Set

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
  | -- | Codata unfold
    EUnfold a t (Label t) Name (List1 (Pattern a t)) (Dictionary (Expression a t)) (Maybe (Expression a t))
  | -- | Record field selector
    ESelect a (Label t) (Expression a t)
  | -- | Codata field selector
    ECodataSelect a (Label t) (Expression a t) (Maybe (Expression a t))
  | -- | Codata internals
    ECodataFields a t (Dictionary (Expression a t))
  | -- | Row restriction
    EFocus Name (Label t) (Label t) (Expression a t) (Expression a t)
  | -- | Trait dictionary placeholder
    EPlaceholder a t (Trait t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

instance (Ord t, Data a, Data t) => FreeVars (Expression a t) t where
  freeIn =
    \case
      EConstructor{} ->
        mempty
      ELambda _ ps e ->
        freeIn e `exceptNames` boundIn ps
      ELet _ gs e1 ->
        freeIn gs <> (freeIn e1 `exceptNames` boundIn gs)
      ERecursiveLet _ p e1 e2 ->
        (freeIn e1 <> freeIn e2) `exceptNames` boundIn p
      EMatch _ _ e cs ->
        freeIn e <> freeIn cs
      ECompiledMatch _ _ e cs ->
        freeIn e <> freeIn cs
      EFocus{} ->
        error "TODO"
      e ->
        Set.fromList (universeBi e)

data Clause a t = EClause a (Pattern a t) (List1 (Choice Expression a t))
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

instance (Ord t, Data a, Data t) => FreeVars (Clause a t) t where
  freeIn =
    \case
      EClause _ p cs ->
        freeIn cs `exceptNames` boundIn p

data CompiledClause a t = ECompiledClause (List1 (Label t)) (Expression a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

instance (Ord t, Data a, Data t) => FreeVars (CompiledClause a t) t where
  freeIn =
    \case
      ECompiledClause (_ :| lls) e ->
        freeIn e `exceptNames` boundIn lls
