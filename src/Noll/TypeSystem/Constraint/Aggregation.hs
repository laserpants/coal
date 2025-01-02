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
  instantiateAnnotation,
) where

import Control.Monad.RWS (
  MonadRWS,
  MonadReader,
  MonadState,
  MonadWriter,
  RWS,
  asks,
  evalRWS,
  get,
  local,
  put,
 )
import Control.Monad.State (StateT, evalStateT, gets)
import Control.Monad.Trans (lift)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
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
  Row (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
  TypeParam (..),
  foldKind,
  foldType,
  kindOf,
  typeIndexesIn,
 )
import Noll.Library.Environment (Environment (..))
import qualified Noll.Library.Environment as Environment
import Noll.Library.List1 (List1, NonEmpty ((:|)), fromList1)
import qualified Noll.Library.List1 as List1
import Noll.TypeSystem.Constraint (Constraint (..), MonomorphicSet (..), overMonomorphicSet)
import Noll.TypeSystem.Constraint.Rule (Assumption (..), InferenceRule (..), assumptionNameIs)
import Noll.Utils (Dictionary, Name, concatMapM, forM, tellLeft, tellRight)

data AggregationError a
  = MissingDataConstructor a Name
  | DataConstructorArityMismatch a Name Int Int
  | IllFormedTypeAnnotation a
  deriving (Show, Eq, Ord, Read)

type AggregationOutput a o k t =
  Either (AggregationError a) (Constraint (InferenceRule k a) o k t)

data AggregationContext o k t = AggregationContext
  { aggregationMonomorphicSet :: MonomorphicSet (o k)
  , aggregationDataConstructorEnv :: Environment (Constructor o k t)
  , aggregationTypeConstructorEnv :: Environment k
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

{-# INLINE lookupDataConstructor #-}
lookupDataConstructor :: Name -> AggregationStack a o k t (Maybe (Constructor o k t))
lookupDataConstructor name = Environment.lookup name <$> asks aggregationDataConstructorEnv

{-# INLINE lookupTypeConstructor #-}
lookupTypeConstructor :: Name -> AggregationStack a o k t (Maybe k)
lookupTypeConstructor name = Environment.lookup name <$> asks aggregationTypeConstructorEnv

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
      r <- lookupDataConstructor name
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
      a <- instantiateAnnotation t
      case a of
        Nothing ->
          tellLeft [IllFormedTypeAnnotation loc]
        Just s ->
          tellRight [Explicit (InferAnnotation loc s) (typeOf e) s]
      aggregateConstraints e
    EConstructor loc (Label t name) -> do
      r <- lookupDataConstructor name
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

instantiateAnnotation :: Type TypeParam () -> ConstraintsAggregation a (Maybe (Scheme TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))))
instantiateAnnotation t = do
  s <- evalStateT (insertKinds =<< translateToIndexed t) (0, mempty)
  pure (Just (Forall (typeIndexesIn s) [] s))

type Instantiate a = StateT (Int, Dictionary (Type TypeIndex ())) (ConstraintsAggregation a)

insertKinds :: Type TypeIndex () -> Instantiate a (Type TypeIndex (Kind KindIndex))
insertKinds =
  \case
    TApplication _ (TVariable (TypeIndex _ n)) ts -> do
      ts1 <- traverse insertKinds ts
      let k = foldKind KType (kindOf <$> ts1)
      pure (TApplication KType (TVariable (TypeIndex k n)) ts1)
    TApplication _ t ts ->
      TApplication KType <$> insertKinds t <*> traverse insertKinds ts
    TArrow t1 t2 ->
      TArrow <$> insertKinds t1 <*> insertKinds t2
    TConstructor _ name -> do
      c <- lift (lookupTypeConstructor name)
      case c of
        Nothing ->
          error "TODO"
        Just k ->
          pure (TConstructor k name)
    TIntrinsic t ->
      TIntrinsic <$> traverse insertKinds t
    TRow row ->
      TRow <$> insertKindsRow row
    TVariable (TypeIndex _ n) ->
      pure (TVariable (TypeIndex KType n))
    TAlias name ts t ->
      TAlias name <$> traverse insertKinds ts <*> insertKinds t

insertKindsRow :: Row TypeIndex () (Type TypeIndex ()) -> Instantiate a (Row TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex)))
insertKindsRow =
  \case
    RExtend name t row ->
      RExtend name <$> insertKinds t <*> insertKindsRow row
    RVariable (TypeIndex _ n) -> do
      pure (RVariable (TypeIndex KRow n))
    RNil ->
      pure RNil

--

translateToIndexed :: Type TypeParam () -> Instantiate a (Type TypeIndex ())
translateToIndexed =
  \case
    TApplication _ t ts ->
      TApplication () <$> translateToIndexed t <*> traverse translateToIndexed ts
    TArrow t1 t2 ->
      TArrow <$> translateToIndexed t1 <*> translateToIndexed t2
    TConstructor _ name ->
      pure (TConstructor () name)
    TIntrinsic t ->
      TIntrinsic <$> traverse translateToIndexed t
    TRow row ->
      TRow <$> translateToIndexedRow row
    TVariable (TypeParam _ name) -> do
      dict <- gets snd
      case Map.lookup name dict of
        Nothing -> do
          nextVar name id TVariable
        Just t@TVariable{} ->
          pure t
        Just _ ->
          error "TODO"
    TAlias name ts t ->
      TAlias name
        <$> traverse translateToIndexed ts
        <*> translateToIndexed t

translateToIndexedRow :: Row TypeParam () (Type TypeParam ()) -> Instantiate a (Row TypeIndex () (Type TypeIndex ()))
translateToIndexedRow =
  \case
    RExtend name t row ->
      RExtend name
        <$> translateToIndexed t
        <*> translateToIndexedRow row
    RVariable (TypeParam _ name) -> do
      dict <- gets snd
      case Map.lookup name dict of
        Nothing ->
          nextVar name TRow RVariable
        Just (TRow row) ->
          pure row
        Just _ ->
          error "TODO"
    RNil ->
      pure RNil

nextVar :: Name -> (t -> Type TypeIndex ()) -> (TypeIndex () -> t) -> Instantiate a t
nextVar name fn con = do
  (n, dict) <- get
  let t = con (TypeIndex () n)
  put (succ n, Map.insert name (fn t) dict)
  pure t
