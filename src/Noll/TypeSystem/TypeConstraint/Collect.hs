{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeConstraint.Collect (
  TypeConstraintsContext (..),
  TypeConstraints (..),
  CollectConstraints,
  collectTypeConstraints,
  runCollectTypeConstraints,
  evalCollectTypeConstraints,
  withMonomorphic,
)
where

import Control.Monad (join)
import Control.Monad.RWS (MonadRWS, MonadReader, MonadState, MonadWriter, RWS, ask, asks, evalRWS, local, tell)
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.Trans (lift)
import Data.List (partition, transpose)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra (second, third3)
import Debug.Trace
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  BoundNames (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  HasType (..),
  Intrinsic (..),
  Kind (..),
  KindIndex (..),
  KindRep (..),
  Pattern (..),
  Row (..),
  Type (..),
  TypeId (..),
  TypeIndex (..),
  TypeIndexed (..),
  foldType,
 )
import Noll.Language.Type.Scheme (Scheme (..))
import Noll.Library.Environment (Environment (..))
import qualified Noll.Library.Environment as Environment
import Noll.Library.Supply (supply, supplyN)
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..), TypeConstraintMetadata (..), overMonomorphicSet)
import Noll.TypeSystem.TypeConstraint.Assumption (Assumption (..), assumptionNameIs)
import Noll.Utils (Dictionary, Name, concatMapM, forM, forM_, (<$$>))

data TypeConstraintsContext o k = TypeConstraintsContext
  { contextMonomorphicSet :: MonomorphicSet (o k)
  , contextConstructorEnv :: Environment (Constructor o k (Type o k))
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overContextMonomorphicSet #-}
overContextMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> TypeConstraintsContext o k -> TypeConstraintsContext o k
overContextMonomorphicSet fn TypeConstraintsContext{..} = TypeConstraintsContext{contextMonomorphicSet = fn contextMonomorphicSet, ..}

type TypeConstraintsMonad c o k t = RWS (TypeConstraintsContext o k) [TypeConstraint c o k t] Int

newtype TypeConstraints c o k t a = TypeConstraints {constraintsMonad :: TypeConstraintsMonad c o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (TypeConstraintsContext o k)
    , MonadWriter [TypeConstraint c o k t]
    , MonadState Int
    , MonadRWS (TypeConstraintsContext o k) [TypeConstraint c o k t] Int
    )

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMany #-}
monosetInsertMany :: (Ord k, Foldable f) => f (TypeIndex k) -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsertMany = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> TypeConstraints c o k t a -> TypeConstraints c o k t a
localMonoset = local . overContextMonomorphicSet

{-# INLINE lookupContextConstructor #-}
lookupContextConstructor :: Name -> TypeConstraints c o k t (Maybe (Constructor o k (Type o k)))
lookupContextConstructor name = Environment.lookup name <$> asks contextConstructorEnv

type CollectConstraints c k = TypeConstraints c TypeIndex k (Type TypeIndex k)

{-# INLINE runCollectTypeConstraints #-}
runCollectTypeConstraints :: Int -> TypeConstraintsContext o k -> TypeConstraints c o k t a -> (a, [TypeConstraint c o k t])
runCollectTypeConstraints n cc cs = evalRWS (constraintsMonad cs) cc n

{-# INLINE evalCollectTypeConstraints #-}
evalCollectTypeConstraints :: Int -> TypeConstraintsContext o k -> TypeConstraints c o k t a -> [TypeConstraint c o k t]
evalCollectTypeConstraints n = snd <$$> runCollectTypeConstraints n

{-# INLINE assertEquality #-}
assertEquality :: TypeConstraintMetadata k a -> [Type TypeIndex k] -> CollectConstraints (TypeConstraintMetadata k a) k ()
assertEquality meta ts = tell [Equality meta ts]

assertEqualityAssumptions :: Type TypeIndex k -> [Assumption (Type TypeIndex k)] -> CollectConstraints (TypeConstraintMetadata k a) k ()
assertEqualityAssumptions t ms =
  tell $ do
    Assumption{..} <- ms
    -- TODO
    pure (Equality TypeConstraintMetadata [assumptionType, t])

assertImplicitAssumptions :: Type TypeIndex k -> [Assumption (Type TypeIndex k)] -> CollectConstraints (TypeConstraintMetadata k a) k ()
assertImplicitAssumptions t ms = do
  set <- asks contextMonomorphicSet
  tell $ do
    Assumption{..} <- ms
    -- TODO
    pure (Implicit TypeConstraintMetadata assumptionType t set)

patternAssumptions :: [Assumption (Type TypeIndex k)] -> Pattern a (Type TypeIndex k) -> CollectConstraints (TypeConstraintMetadata k a) k [Assumption (Type TypeIndex k)]
patternAssumptions ms =
  \case
    PVariable _ (Label t name) -> do
      let (ls, rs) = partition (assumptionNameIs name) ms
      assertEqualityAssumptions t ls
      pure rs

withMonomorphic ::
  (Ord k, TypeIndexed k s, KindRep k) =>
  s ->
  TypeConstraints c TypeIndex k t a ->
  TypeConstraints c TypeIndex k t a
withMonomorphic ps = localMonoset (monosetInsertMany (typeIndexesIn ps))

collectTypeConstraints ::
  (Ord k, Show k, KindRep k) =>
  Expression a (Type TypeIndex k) ->
  CollectConstraints (TypeConstraintMetadata k a) k [Assumption (Type TypeIndex k)]
collectTypeConstraints =
  \case
    EAnnotation a e -> do
      s <- annotationScheme a
      tell [Explicit TypeConstraintMetadata (typeOf e) s]
      collectTypeConstraints e
    EConstructor _ (Label t name) -> do
      r <- lookupContextConstructor name
      case r of
        Nothing ->
          error ("No constructor '" <> Text.unpack name <> "'")
        Just Constructor{..} ->
          tell [Explicit TypeConstraintMetadata t constructorScheme]
      pure []
    EVariable _ (Label t name) ->
      pure [Assumption name t]
    ELambda _ ps e -> do
      ms1 <- withMonomorphic ps (collectTypeConstraints e)
      ms2 <- concat <$> forM ps (patternAssumptions ms1)
      pure ms2
    ELet _ gs e1 -> do
      ms1 <- collectTypeConstraints e1
      ms2 <- flip concatMapM gs $
        \case
          BPattern _ (PVariable _ (Label t name)) e -> do
            ms <- collectTypeConstraints e
            -- TODO
            assertEquality TypeConstraintMetadata [t, typeOf e]
            pure ms
      ms3 <- flip concatMapM gs $
        \case
          BPattern _ (PVariable _ (Label t name)) e -> do
            let (ls, rs) = partition (assumptionNameIs name) ms1
            assertImplicitAssumptions t ls
            pure rs
      pure (ms1 <> ms2 <> ms3)
    EIf loc t e1 e2 e3 -> do
      ms1 <- collectTypeConstraints e1
      ms2 <- collectTypeConstraints e2
      ms3 <- collectTypeConstraints e3
      let t1 = typeOf e1
          t2 = typeOf e2
          t3 = typeOf e3
      assertEquality (ConstraintIfCondition loc) [t1, TIntrinsic IBool]
      assertEquality (ConstraintIfBranches loc t2 t3) [t, t2, t3]
      pure (ms1 <> ms2 <> ms3)
    EApplication loc t e1 es -> do
      ms1 <- collectTypeConstraints e1
      ms2 <- concat <$> traverse collectTypeConstraints es
      let t1 = typeOf e1
          t2 = foldType t (typeOf <$> es)
      assertEquality (ConstraintApplication loc t1 t2) [t1, t2]
      pure (ms1 <> ms2)
    ELiteral{} ->
      pure []
    EMatch loc t es cs -> do
      ms1 <- concat <$> traverse collectTypeConstraints (NonEmpty.toList es)
      (ets, pts, ms2) <- third3 concat . unzip3 <$> forM (NonEmpty.toList cs) collectClauseTypeConstraints
      -- Expression types
      assertEquality (ConstraintMatchClauseExpressions loc) (t : concat ets)
      -- Pattern types
      forM
        (transpose (NonEmpty.toList (typeOf <$> es) : pts))
        (assertEquality (ConstraintMatchClausePatterns loc))
      pure (ms1 <> ms2)

freshType :: (Ord k, Show k, KindRep k) => CollectConstraints (TypeConstraintMetadata k a) k (Type TypeIndex k)
freshType = do
  i <- supply
  pure (TVariable (TypeIndex (kindRep (KVariable (KindIndex i))) i))

assertBound ::
  (Ord k, Show k, KindRep k) =>
  (Dictionary (Type TypeIndex k)) ->
  [Assumption (Type TypeIndex k)] ->
  CollectConstraints (TypeConstraintMetadata k a) k [Assumption (Type TypeIndex k)]
assertBound dict ms =
  concat <$$> forM ms $
    \a@Assumption{..} ->
      case Map.lookup assumptionName dict of
        Nothing ->
          pure [a]
        Just t -> do
          assertEquality TypeConstraintMetadata [t, assumptionType]
          pure []

collectClauseTypeConstraints ::
  (Ord k, Show k, KindRep k) =>
  Clause Expression a (Type TypeIndex k) ->
  CollectConstraints (TypeConstraintMetadata k a) k ([Type TypeIndex k], [Type TypeIndex k], [Assumption (Type TypeIndex k)])
collectClauseTypeConstraints (EClause _ ps cs) = do
  (ts1, ms1) <- second concat . unzip <$$> withMonomorphic ps $
    forM (NonEmpty.toList cs) $
      \case
        CPlain _ gs e -> do
          forM gs $ \g ->
            assertEquality ConstraintMatchClauseGuard [typeOf g, TIntrinsic IBool]
          ms <- collectTypeConstraints e
          pure (typeOf e, ms)
  (ts2, ms2) <- second concat . unzip <$$> forM (NonEmpty.toList ps) $
    \case
      PVariable _ (Label t name) -> do
        undefined
      PConstructor _ (Label t name) qs -> do
        t1 <- freshType
        -- TODO: Check for duplicate names in patterns
        let dict = Map.fromList (boundNames qs)
        ms <- assertBound dict ms1
        lookupContextConstructor name
          >>= \case
            Nothing ->
              error ("No constructor '" <> Text.unpack name <> "'")
            Just Constructor{..}
              | constructorArity /= length qs ->
                  error ("Constructor arity mismatch")
            Just Constructor{..} -> do
              assertEquality TypeConstraintMetadata [t, foldType t1 (typeOf <$> qs)]
              tell [Explicit TypeConstraintMetadata t constructorScheme]
        pure (t1, ms)
  pure (ts1, ts2, ms1 <> ms2)

annotationScheme :: (MonadState Int m, Ord k, KindRep k) => Type TypeId () -> m (Scheme TypeIndex k (Type TypeIndex k))
annotationScheme t = do
  s <- evalStateT (instantiateAnnotation t) mempty
  pure (Forall (typeIndexesIn s) [] s)

type Annotation m k = StateT (Dictionary (TypeIndex k)) m

instantiateAnnotation :: (MonadState Int m, KindRep k) => Type TypeId () -> Annotation m k (Type TypeIndex k)
instantiateAnnotation =
  \case
    TApplication _ t ts ->
      TApplication
        <$> freshRep
        <*> instantiateAnnotation t
        <*> traverse instantiateAnnotation ts
    TArrow t1 t2 ->
      TArrow
        <$> instantiateAnnotation t1
        <*> instantiateAnnotation t2
    TConstructor _ name ->
      TConstructor
        <$> freshRep
        <*> pure name
    TIntrinsic t ->
      TIntrinsic
        <$> traverse instantiateAnnotation t
    TRow row ->
      TRow
        <$> instantiateAnnotationRow row
    TVariable v ->
      TVariable
        <$> instantiateAnnotationTypeId v
    TAlias name ts t ->
      TAlias name
        <$> traverse instantiateAnnotation ts
        <*> instantiateAnnotation t

freshRep :: (MonadState Int m, KindRep k) => Annotation m k k
freshRep = do
  i <- lift supply
  pure (kindRep (KVariable (KindIndex i)))

instantiateAnnotationRow :: (MonadState Int m, KindRep k) => Row TypeId () (Type TypeId ()) -> Annotation m k (Row TypeIndex k (Type TypeIndex k))
instantiateAnnotationRow =
  \case
    RExtend name t row ->
      RExtend name
        <$> instantiateAnnotation t
        <*> instantiateAnnotationRow row
    RVariable v ->
      RVariable
        <$> instantiateAnnotationTypeId v
    RNil ->
      pure RNil

instantiateAnnotationTypeId :: (MonadState Int m, KindRep k) => TypeId () -> Annotation m k (TypeIndex k)
instantiateAnnotationTypeId (TypeId _ name) = do
  dict <- get
  case Map.lookup name dict of
    Nothing -> do
      i <- lift supply
      let k = TypeIndex (kindRep (KVariable (KindIndex i))) i
      modify (Map.insert name k)
      pure k
    Just k ->
      pure k
