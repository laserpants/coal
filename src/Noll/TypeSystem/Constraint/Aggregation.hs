{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Aggregation (
  ConstraintsGenerationContext (..),
  ConstraintsGenerationError (..),
  collectConstraints,
  runAggregationStack,
) where

import Control.Monad.Reader (asks)
import Data.List (partition)
import Data.Tuple.Extra (second, third3)
import Debug.Trace
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  Guard (..),
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
import Noll.Library.List1 (fromList1)
import Noll.TypeSystem.Constraint (Constraint (..))
import Noll.TypeSystem.Constraint.Aggregation.Internal (
  ConstraintsGenerationContext (..),
  AggregationStack (..),
  ConstraintsGenerationError (..),
  InferenceRule (..),
  localMonoset,
  monosetInsertMany,
  runAggregationStack,
 )
import Noll.TypeSystem.Constraint.Aggregation.TypeAnnotation (
  instantiateAnnotation,
 )
import Noll.TypeSystem.Constraint.Assumption (
  Assumption (..),
  assumptionNameIs,
  assumptionNameIsNotOneOf,
 )
import Noll.Utils (Name, concatForM, concatMapM, forM, tellLeft, tellRight, (<$$>))

import qualified Noll.Library.Environment as Environment

type ConstraintsAggregation a = AggregationStack a TypeIndex Kind IndexedType

{-# INLINE lookupDataConstructor #-}
lookupDataConstructor :: Name -> AggregationStack c o k t (Maybe (Constructor o k t))
lookupDataConstructor name = asks (Environment.lookup name . aggregationDataConstructorEnv)

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

type Assertion a = IndexedType -> [Assumption IndexedType] -> ConstraintsAggregation a ()

patternConstraints ::
  Assertion a ->
  [Assumption IndexedType] ->
  Pattern a IndexedType ->
  ConstraintsAggregation a [Name]
patternConstraints assert ms =
  \case
    PAnnotation loc t p -> do
      r <- instantiateAnnotation loc t
      case r of
        Left err ->
          tellLeft [IllFormedTypeAnnotation err]
        Right t1 ->
          tellRight [Equality (InferAnnotation loc t1) [typeOf p, t1]]
      patternConstraints assert ms p
    PVariable _ (Label t name) -> do
      assert t (filter (assumptionNameIs name) ms)
      pure [name]
    PConstructor loc (Label t name) ps -> do
      r <- lookupDataConstructor name
      case r of
        Nothing ->
          tellLeft [NoDataConstructor loc name]
        Just Constructor{..}
          | constructorArity /= length ps ->
              tellLeft [DataConstructorArityMismatch loc name constructorArity (length ps)]
        Just Constructor{..} ->
          tellRight [Explicit (InferenceRule 3) (foldType t (typeOf <$> ps)) constructorScheme]
      concatForM ps (patternConstraints assert ms)

clauseAssumptions ::
  Clause Expression a IndexedType ->
  ConstraintsAggregation a (IndexedType, [IndexedType], [Assumption IndexedType])
clauseAssumptions (EClause loc p cs) = do
  (ts1, ms) <- second concat . unzip <$$> withMonomorphic p $
    forM (fromList1 cs) $
      \case
        CPlain _ gs e -> do
          ms1 <- concatForM gs $ \(CGuard g) -> do
            tellRight [Equality (InferMatchClauseGuard loc) [typeOf g, TIntrinsic IBool]]
            collectConstraints g
          ms2 <- collectConstraints e
          pure (typeOf e, ms1 <> ms2)
  names <- patternConstraints (assertEqualityAssumptions loc) ms p
  pure (typeOf p, ts1, filter (assumptionNameIsNotOneOf names) ms)

collectConstraints :: Expression a IndexedType -> ConstraintsAggregation a [Assumption IndexedType]
collectConstraints =
  \case
    EAnnotation loc t e -> do
      r <- instantiateAnnotation loc t
      case r of
        Left err ->
          tellLeft [IllFormedTypeAnnotation err]
        Right t1 ->
          tellRight [Equality (InferAnnotation loc t1) [typeOf e, t1]]
      collectConstraints e
    EConstructor loc (Label t name) -> do
      r <- lookupDataConstructor name
      case r of
        Nothing ->
          tellLeft [NoDataConstructor loc name]
        Just Constructor{..} ->
          tellRight [Explicit (InferenceRule 4) t constructorScheme]
      pure []
    EVariable loc (Label t name) ->
      pure [Assumption name t]
    ELambda loc ps e -> do
      ms <- withMonomorphic ps (collectConstraints e)
      names <- concatForM ps (patternConstraints (assertEqualityAssumptions loc) ms)
      pure (filter (assumptionNameIsNotOneOf names) ms)
    ELet loc gs e1 -> do
      ms1 <- collectConstraints e1
      ms2 <- concatForM gs $
        \case
          BPattern _ p e -> do
            let t1 = typeOf p
                t2 = typeOf e
            tellRight [Equality (InferLetBindingPattern loc t1 t2) [t1, t2]]
            collectConstraints e
      names <- concatForM gs $
        \case
          BPattern _ p _ ->
            patternConstraints (assertImplicitAssumptions loc) ms1 p
      pure (filter (assumptionNameIsNotOneOf names) ms1 <> ms2)
    EIf loc t e1 e2 e3 -> do
      ms1 <- collectConstraints e1
      ms2 <- collectConstraints e2
      ms3 <- collectConstraints e3
      let t1 = typeOf e1
          t2 = typeOf e2
          t3 = typeOf e3
      tellRight [Equality (InferIfCondition loc t1) [t1, TIntrinsic IBool]]
      tellRight [Equality (InferIfBranches loc t2 t3) [t, t2, t3]]
      pure (ms1 <> ms2 <> ms3)
    EApplication loc t e1 es -> do
      ms1 <- collectConstraints e1
      ms2 <- concatMapM collectConstraints es
      let t1 = typeOf e1
          t2 = foldType t ts
          ts = typeOf <$> es
      tellRight [Equality (InferApplication loc t1 (fromList1 ts)) [t1, t2]]
      pure (ms1 <> ms2)
    ELiteral{} ->
      pure []
    EMatch loc t e cs -> do
      ms1 <- collectConstraints e
      (ts1, ts2, ms2) <- (third3 concat . unzip3 <$$> traverse clauseAssumptions) (fromList1 cs)
      -- Pattern types
      tellRight [Equality (InferMatchClausePatterns loc) (typeOf e : ts1)]
      -- Expression types
      tellRight [Equality (InferMatchClauseExpressions loc) (t : concat ts2)]
      pure (ms1 <> ms2)
