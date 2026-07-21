{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Language.Expression

Expression grammar for the Coal language AST.
-}
module Coal.Language.Expression (
  Expression (..),
  Clause (..),
  CompiledClause (..),
) where

import Coal.Common.FreeVars
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
import Data.Set (Set)
import qualified Data.Set as Set
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
    EFFICall a t (Label (Type Parameter s)) [Expression a s t] (Expression a s t)
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

data Clause a s t = EClause
  { clauseMetadata :: a
  , clausePattern :: Pattern a s t
  , clauseChoices :: NonEmpty (Choice Expression a s t)
  }
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

data CompiledClause a s t = ECompiledClause
  { compiledClauseMetadata :: a
  , compiledClauseSegments :: NonEmpty (Label t)
  , compiledClauseExpression :: Expression a s t
  }
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

instance (Ord t, Data a, Data s, Data t) => FreeVars (Expression a s t) t where
  freeIn =
    \case
      EAnnotation _ _ e ->
        freeIn e
      EApplication _ _ f args ->
        freeIn f <> freeIn args
      ELambda _ pats body ->
        let bound = boundIn pats
         in freeSet bound body
      ELet _ bindings body ->
        let bound = boundIn bindings
            rhsFree = freeIn bindings
            bodyFree = freeSet bound body
         in rhsFree <> bodyFree
      ERecursiveLet _ pat rhs body ->
        let bound = boundIn pat
            rhsFree = freeSet bound rhs
            bodyFree = freeSet bound body
         in rhsFree <> bodyFree
      EVariable _ lbl ->
        Set.singleton lbl
      EConstructor{} ->
        mempty
      ELiteral{} ->
        mempty
      EIf _ _ c t f ->
        freeIn c <> freeIn t <> freeIn f
      EOperator{} ->
        mempty
      ERecord _ _ fields rest ->
        freeIn fields <> freeIn rest
      EListCons _ _ x xs ->
        freeIn x <> freeIn xs
      EListLiteral _ _ xs ->
        freeIn xs
      ETuple _ _ xs ->
        freeIn xs
      EMatch _ _ e clauses ->
        freeIn e <> foldMap freeClause clauses
      ELambdaMatch _ _ clauses ->
        foldMap freeClause clauses
      ECompiledMatch _ _ e clauses ->
        freeIn e <> foldMap freeCompiledClause clauses
      EFold _ _ exprs clauses ->
        freeIn exprs <> foldMap freeClause clauses
      ESelect _ _ e ->
        freeIn e
      EFocus _ _ _ _ e1 e2 ->
        freeIn e1 <> freeIn e2
      ETraitInstance{} ->
        mempty
      EFFICall _ _ _ args k ->
        freeIn args <> freeIn k
      _ ->
        error "Not implemented"

freeClause ::
  (Ord t, Data a, Data s, Data t) =>
  Clause a s t ->
  Set (Label t)
freeClause (EClause _ pat choices) =
  let bound = boundIn pat
   in foldMap (freeChoice bound) choices

freeChoice ::
  (Ord t, Data a, Data s, Data t) =>
  Set Name ->
  Choice Expression a s t ->
  Set (Label t)
freeChoice bound (CPlain _ guards expr) =
  freeIn guards <> freeSet bound expr

freeCompiledClause ::
  (Ord t, Data a, Data s, Data t) =>
  CompiledClause a s t ->
  Set (Label t)
freeCompiledClause (ECompiledClause _ _ expr) =
  freeIn expr
