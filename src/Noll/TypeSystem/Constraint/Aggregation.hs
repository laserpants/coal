{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Aggregation (
  AggregationContext (..),
  aggregateConstraints,
  runAggregationStack,
) where

import Control.Monad.Reader (asks)
import Data.List (partition)
import Debug.Trace
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Constructor (..),
  Expression (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Pattern (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
  foldType,
  typeOf,
 )
import qualified Noll.Library.Environment as Environment
import Noll.Library.List1 (fromList1)
import Noll.TypeSystem.Constraint (Constraint (..))
import Noll.TypeSystem.Constraint.Aggregation.Internal (
  AggregationContext (..),
  AggregationError (..),
  AggregationStack (..),
  localMonoset,
  monosetInsertMany,
  runAggregationStack,
 )
import Noll.TypeSystem.Constraint.Aggregation.TypeAnnotation (instantiateAnnotation)
import Noll.TypeSystem.Constraint.Rule (Assumption (..), InferenceRule (..), assumptionNameIs)
import Noll.Utils (Name, concatMapM, forM, tellLeft, tellRight)

type ConstraintsAggregation a = AggregationStack a TypeIndex Kind IndexedType

{-# INLINE lookupDataConstructor #-}
lookupDataConstructor :: Name -> AggregationStack w o k t (Maybe (Constructor o k t))
lookupDataConstructor name = Environment.lookup name <$> asks aggregationDataConstructorEnv

assertEqualityAssumptions :: a -> IndexedType -> [Assumption IndexedType] -> ConstraintsAggregation a ()
assertEqualityAssumptions loc t ms =
  tellRight $ do
    Assumption{..} <- ms
    -- TODO
    pure (Equality (InferenceRule 1) [assumptionType, t])

assertImplicitAssumptions :: a -> IndexedType -> [Assumption IndexedType] -> ConstraintsAggregation a ()
assertImplicitAssumptions loc t ms = do
  set <- asks aggregationMonomorphicSet
  tellRight $ do
    Assumption{..} <- ms
    -- TODO
    pure (Implicit (InferLetImplicit loc assumptionName assumptionType t) assumptionType t set)

withMonomorphic :: (TypeIndexed Kind t) => t -> ConstraintsAggregation a c -> ConstraintsAggregation a c
withMonomorphic a = localMonoset (monosetInsertMany (typeIndexesIn a))

type Assert a = IndexedType -> [Assumption IndexedType] -> ConstraintsAggregation a ()

patternAssumptions ::
  Assert a ->
  [Assumption IndexedType] ->
  Pattern a IndexedType ->
  ConstraintsAggregation a [Assumption IndexedType]
patternAssumptions assert ms =
  \case
    PVariable _ (Label t name) -> do
      let (ls, rs) = partition (assumptionNameIs name) ms
      assert t ls
      pure rs
    PConstructor loc (Label t name) ps -> do
      r <- lookupDataConstructor name
      case r of
        Nothing ->
          tellLeft [MissingDataConstructor loc name]
        Just Constructor{..}
          | constructorArity /= length ps ->
              tellLeft [DataConstructorArityMismatch loc name constructorArity (length ps)]
        Just Constructor{..} ->
          tellRight [Explicit (InferenceRule 3) (foldType t (typeOf <$> ps)) constructorScheme]
      concat <$> traverse (patternAssumptions assert ms) ps

aggregateConstraints ::
  Expression a IndexedType ->
  ConstraintsAggregation a [Assumption IndexedType]
aggregateConstraints =
  \case
    EAnnotation loc t e -> do
      r <- instantiateAnnotation t
      case r of
        Left err ->
          tellLeft [IllFormedTypeAnnotation loc err]
        Right s ->
          tellRight [Explicit (InferAnnotation loc s) (typeOf e) s]
      aggregateConstraints e
    EConstructor loc (Label t name) -> do
      r <- lookupDataConstructor name
      case r of
        Nothing ->
          tellLeft [MissingDataConstructor loc name]
        Just Constructor{..} ->
          tellRight [Explicit (InferenceRule 4) t constructorScheme]
      pure []
    EVariable loc (Label t name) ->
      pure [Assumption name t]
    ELambda loc ps e -> do
      ms1 <- withMonomorphic ps (aggregateConstraints e)
      concat <$> forM ps (patternAssumptions (assertEqualityAssumptions loc) ms1)
    ELet loc gs e1 -> do
      ms1 <- aggregateConstraints e1
      ms2 <- flip concatMapM gs $
        \case
          BPattern _ p e -> do
            ms <- aggregateConstraints e
            let t1 = typeOf p
                t2 = typeOf e
            tellRight [Equality (InferLetBindingPattern loc t1 t2) [t1, t2]]
            pure ms
      ms3 <- flip concatMapM gs $
        \case
          BPattern _ p _ ->
            patternAssumptions (assertImplicitAssumptions loc) ms1 p
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
