{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Aggregation (aggregateConstraints) where

import Control.Monad.RWS (MonadRWS, MonadReader, MonadState, MonadWriter, RWS)
import Noll.Label (Label (..))
import Noll.Language (
  Constructor (..),
  Expression (..),
  Kind (..),
  KindIndex,
  Type (..),
  TypeIndex (..),
 )
import Noll.Library.Environment (Environment (..))
import Noll.TypeSystem.Constraint (Constraint (..), MonomorphicSet (..))
import Noll.TypeSystem.Constraint.Rule (Assumption (..), InferenceRule (..))
import Noll.Utils (Name)

data AggregationError a
  = MissingDataConstructor a Name
  | DataConstructorArityMismatch a Name Int Int
  | IllFormedTypeAnnotation a
  deriving (Show, Eq, Ord, Read)

data AggregationOutput a o k t
  = Either (AggregationError a) (Constraint (InferenceRule k a) o k t)

data AggregationContext o k t = AggregationContext
  { aggregationMonomorphicSet :: MonomorphicSet (o k)
  , aggregationConstructorEnv :: Environment (Constructor o k t)
  }
  deriving (Show, Eq, Ord, Read)

type AggregationMonad a o k t = RWS (AggregationContext o k t) [AggregationOutput a o k t] ()

newtype AggregationStack a o k t c = AggregationStack {aggregationMonad :: AggregationMonad a o k t c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AggregationContext o k t)
    , MonadWriter [AggregationOutput a o k t]
    , MonadState ()
    , MonadRWS (AggregationContext o k t) [AggregationOutput a o k t] ()
    )

type Aggregation a = AggregationStack a TypeIndex (Kind Int) (Type TypeIndex (Kind KindIndex))

aggregateConstraints ::
  Expression a (Type TypeIndex (Kind KindIndex)) ->
  Aggregation a [Assumption (Type TypeIndex (Kind KindIndex))]
aggregateConstraints =
  \case
    EAnnotation loc t e -> do
      undefined
    EConstructor loc (Label t name) -> do
      undefined
    EVariable loc (Label t name) ->
      undefined
    ELambda loc ps e -> do
      undefined
    ELet loc gs e1 -> do
      undefined
    EIf loc t e1 e2 e3 -> do
      undefined
    EApplication loc t e1 es -> do
      undefined
    ELiteral{} ->
      undefined
    EMatch loc t e cs -> do
      undefined
