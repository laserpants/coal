{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Aggregation (
  AggregationContext (..),
  AggregationOutput (..),
  aggregateConstraints,
  runAggregationStack,
) where

import Control.Monad.RWS (
  MonadRWS,
  MonadReader,
  MonadState,
  MonadWriter,
  RWS,
  asks,
  evalRWS,
  local,
 )
import Data.List (partition)
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Constructor (..),
  Expression (..),
  HasType (..),
  Intrinsic (..),
  Kind (..),
  KindIndex,
  Pattern (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
  foldType,
  typeIndexesIn,
 )
import Noll.Library.Environment (Environment (..))
import qualified Noll.Library.Environment as Environment
import Noll.Library.List1 (fromList1)
import Noll.TypeSystem.Constraint (Constraint (..), MonomorphicSet (..), overMonomorphicSet)
import Noll.TypeSystem.Constraint.Rule (Assumption (..), InferenceRule (..), assumptionNameIs)
import Noll.Utils (Name, concatMapM, forM, tellLeft, tellRight)

data AggregationError a
  = MissingDataConstructor a Name
  | DataConstructorArityMismatch a Name Int Int
  | IllFormedTypeAnnotation a
  deriving (Show, Eq, Ord, Read)

type AggregationOutput a o k t =
  Either (AggregationError a) (Constraint (InferenceRule k a) o k t)

data AggregationContext o k t = AggregationContext
  { aggregationMonomorphicSet :: MonomorphicSet (o k)
  , aggregationConstructorEnv :: Environment (Constructor o k t)
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overAggregationMonomorphicSet #-}
overAggregationMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> AggregationContext o k t -> AggregationContext o k t
overAggregationMonomorphicSet fn AggregationContext{..} = AggregationContext{aggregationMonomorphicSet = fn aggregationMonomorphicSet, ..}

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

{-# INLINE runAggregationStack #-}
runAggregationStack :: AggregationContext o k t -> AggregationStack a o k t c -> (c, [AggregationOutput a o k t])
runAggregationStack ctx m = evalRWS (aggregationMonad m) ctx ()

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMany #-}
monosetInsertMany :: (Ord k, Foldable f) => f (TypeIndex k) -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsertMany = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> AggregationStack a o k t c -> AggregationStack a o k t c
localMonoset = local . overAggregationMonomorphicSet

{-# INLINE lookupContextConstructor #-}
lookupContextConstructor :: Name -> AggregationStack a o k t (Maybe (Constructor o k t))
lookupContextConstructor name = Environment.lookup name <$> asks aggregationConstructorEnv

type ConstraintsAggregation a =
  AggregationStack a TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))

assertEqualityAssumptions :: Type TypeIndex (Kind KindIndex) -> [Assumption (Type TypeIndex (Kind KindIndex))] -> ConstraintsAggregation a ()
assertEqualityAssumptions t ms =
  tellRight $ do
    Assumption{..} <- ms
    -- TODO
    pure (Equality InferenceRule [assumptionType, t])

assertImplicitAssumptions :: Type TypeIndex (Kind KindIndex) -> [Assumption (Type TypeIndex (Kind KindIndex))] -> ConstraintsAggregation a ()
assertImplicitAssumptions t ms = do
  set <- asks aggregationMonomorphicSet
  tellRight $ do
    Assumption{..} <- ms
    -- TODO
    pure (Implicit InferenceRule assumptionType t set)

type Assert a = Type TypeIndex (Kind KindIndex) -> [Assumption (Type TypeIndex (Kind KindIndex))] -> ConstraintsAggregation a ()

patternAssumptions ::
  Assert a ->
  [Assumption (Type TypeIndex (Kind KindIndex))] ->
  Pattern a (Type TypeIndex (Kind KindIndex)) ->
  ConstraintsAggregation a [Assumption (Type TypeIndex (Kind KindIndex))]
patternAssumptions assert ms =
  \case
    PVariable _ (Label t name) -> do
      let (ls, rs) = partition (assumptionNameIs name) ms
      assert t ls
      pure rs
    PConstructor loc (Label t name) ps -> do
      r <- lookupContextConstructor name
      case r of
        Nothing ->
          tellLeft [MissingDataConstructor loc name]
        Just Constructor{..}
          | constructorArity /= length ps ->
              tellLeft [DataConstructorArityMismatch loc name constructorArity (length ps)]
        Just Constructor{..} ->
          tellRight [Explicit InferenceRule (foldType t (typeOf <$> ps)) constructorScheme]
      concat <$> traverse (patternAssumptions assert ms) ps

withMonomorphic :: (TypeIndexed (Kind KindIndex) t) => t -> ConstraintsAggregation a c -> ConstraintsAggregation a c
withMonomorphic a = localMonoset (monosetInsertMany (typeIndexesIn a))

aggregateConstraints ::
  Expression a (Type TypeIndex (Kind KindIndex)) ->
  ConstraintsAggregation a [Assumption (Type TypeIndex (Kind KindIndex))]
aggregateConstraints =
  \case
    EAnnotation loc t e -> do
      -- TODO
      aggregateConstraints e
    EConstructor loc (Label t name) -> do
      r <- lookupContextConstructor name
      case r of
        Nothing ->
          tellLeft [MissingDataConstructor loc name]
        Just Constructor{..} ->
          tellRight [Explicit InferenceRule t constructorScheme]
      pure []
    EVariable loc (Label t name) ->
      pure [Assumption name t]
    ELambda loc ps e -> do
      ms1 <- withMonomorphic ps (aggregateConstraints e)
      concat <$> forM ps (patternAssumptions assertEqualityAssumptions ms1)
    ELet loc gs e1 -> do
      ms1 <- aggregateConstraints e1
      ms2 <- flip concatMapM gs $
        \case
          BPattern _ p e -> do
            ms <- aggregateConstraints e
            tellRight [Equality InferenceRule [typeOf p, typeOf e]]
            pure ms
      ms3 <- flip concatMapM gs $
        \case
          BPattern _ p _ ->
            patternAssumptions assertImplicitAssumptions ms1 p
      pure (ms1 <> ms2 <> ms3)
    EIf loc t e1 e2 e3 -> do
      ms1 <- aggregateConstraints e1
      ms2 <- aggregateConstraints e2
      ms3 <- aggregateConstraints e3
      let t1 = typeOf e1
          t2 = typeOf e2
          t3 = typeOf e3
      tellRight [Equality (InferIfCondition loc t1) [t1, (TIntrinsic IBool)]]
      tellRight [Equality (InferIfBranches loc t2 t3) [t, t2, t3]]
      pure (ms1 <> ms2 <> ms3)
    EApplication loc t e1 es -> do
      ms1 <- aggregateConstraints e1
      ms2 <- concat <$> traverse aggregateConstraints es
      let t1 = typeOf e1
          t2 = foldType t ts
          ts = typeOf <$> es
      tellRight [Equality (InferApplication loc t1 (fromList1 ts)) [t1, t2]]
      pure (ms1 <> ms2)
    ELiteral{} ->
      pure []
    EMatch loc t e cs -> do
      undefined
