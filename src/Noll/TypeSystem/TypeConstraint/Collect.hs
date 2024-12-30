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
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra (second, third3)
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  Guard (..),
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
import Noll.TypeSystem.TypeConstraint (Descriptor (..), MonomorphicSet (..), TypeConstraint (..), overMonomorphicSet)
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

type TypeConstraintsMonad y o k t = RWS (TypeConstraintsContext o k) [TypeConstraint y o k t] Int

newtype TypeConstraints y o k t a = TypeConstraints {constraintsMonad :: TypeConstraintsMonad y o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (TypeConstraintsContext o k)
    , MonadWriter [TypeConstraint y o k t]
    , MonadState Int
    , MonadRWS (TypeConstraintsContext o k) [TypeConstraint y o k t] Int
    )

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMany #-}
monosetInsertMany :: (Ord k, Foldable f) => f (TypeIndex k) -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsertMany = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> TypeConstraints y o k t a -> TypeConstraints y o k t a
localMonoset = local . overContextMonomorphicSet

{-# INLINE lookupContextConstructor #-}
lookupContextConstructor :: Name -> TypeConstraints y o k t (Maybe (Constructor o k (Type o k)))
lookupContextConstructor name = Environment.lookup name <$> asks contextConstructorEnv

type CollectConstraints c k = TypeConstraints c TypeIndex k (Type TypeIndex k)

{-# INLINE runCollectTypeConstraints #-}
runCollectTypeConstraints :: Int -> TypeConstraintsContext o k -> TypeConstraints y o k t a -> (a, [TypeConstraint y o k t])
runCollectTypeConstraints n ctx cs = evalRWS (constraintsMonad cs) ctx n

{-# INLINE evalCollectTypeConstraints #-}
evalCollectTypeConstraints :: Int -> TypeConstraintsContext o k -> TypeConstraints y o k t a -> [TypeConstraint y o k t]
evalCollectTypeConstraints n = snd <$$> runCollectTypeConstraints n

{-# INLINE assertEquality #-}
assertEquality :: Descriptor k a -> [Type TypeIndex k] -> CollectConstraints (Descriptor k a) k ()
assertEquality meta ts = tell [Equality meta ts]

assertEqualityAssumptions :: Type TypeIndex k -> [Assumption (Type TypeIndex k)] -> CollectConstraints (Descriptor k a) k ()
assertEqualityAssumptions t ms =
  tell $ do
    Assumption{..} <- ms
    -- TODO
    pure (Equality Descriptor [assumptionType, t])

assertImplicitAssumptions :: Type TypeIndex k -> [Assumption (Type TypeIndex k)] -> CollectConstraints (Descriptor k a) k ()
assertImplicitAssumptions t ms = do
  set <- asks contextMonomorphicSet
  tell $ do
    Assumption{..} <- ms
    -- TODO
    pure (Implicit Descriptor assumptionType t set)

patternAssumptions ::
  (Ord k, Show k, KindRep k) =>
  [Assumption (Type TypeIndex k)] ->
  Pattern a (Type TypeIndex k) ->
  CollectConstraints (Descriptor k a) k [Assumption (Type TypeIndex k)]
patternAssumptions ms =
  \case
    PVariable _ (Label t name) -> do
      let (ls, rs) = partition (assumptionNameIs name) ms
      assertEqualityAssumptions t ls
      pure rs
    PConstructor _ (Label t name) ps -> do
      lookupContextConstructor name
        >>= \case
          Nothing ->
            error ("No constructor '" <> Text.unpack name <> "'")
          Just Constructor{..}
            | constructorArity /= length ps ->
                error ("Constructor arity mismatch")
          Just Constructor{..} ->
            tell [Explicit Descriptor (foldType t (typeOf <$> ps)) constructorScheme]
      concat <$> traverse (patternAssumptions ms) ps

withMonomorphic ::
  (Ord k, TypeIndexed k s, KindRep k) =>
  s ->
  TypeConstraints c TypeIndex k t a ->
  TypeConstraints c TypeIndex k t a
withMonomorphic p = localMonoset (monosetInsertMany (typeIndexesIn p))

collectTypeConstraints ::
  (Ord k, Show k, KindRep k) =>
  Expression a (Type TypeIndex k) ->
  CollectConstraints (Descriptor k a) k [Assumption (Type TypeIndex k)]
collectTypeConstraints =
  \case
    EAnnotation a e -> do
      s <- annotationScheme a
      tell [Explicit Descriptor (typeOf e) s]
      collectTypeConstraints e
    EConstructor _ (Label t name) -> do
      lookupContextConstructor name
        >>= \case
          Nothing ->
            error ("No constructor '" <> Text.unpack name <> "'")
          Just Constructor{..} ->
            tell [Explicit Descriptor t constructorScheme]
      pure []
    EVariable _ (Label t name) ->
      pure [Assumption name t]
    ELambda _ ps e -> do
      ms1 <- withMonomorphic ps (collectTypeConstraints e)
      concat <$> forM ps (patternAssumptions ms1)
    ELet _ gs e1 -> do
      ms1 <- collectTypeConstraints e1
      ms2 <- flip concatMapM gs $
        \case
          BPattern _ (PVariable _ (Label t name)) e -> do
            ms <- collectTypeConstraints e
            -- TODO
            assertEquality Descriptor [t, typeOf e]
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
      assertEquality (RuleIfCondition loc) [t1, TIntrinsic IBool]
      assertEquality (RuleIfBranches loc t2 t3) [t, t2, t3]
      pure (ms1 <> ms2 <> ms3)
    EApplication loc t e1 es -> do
      ms1 <- collectTypeConstraints e1
      ms2 <- concat <$> traverse collectTypeConstraints es
      let t1 = typeOf e1
          t2 = foldType t (typeOf <$> es)
      assertEquality (RuleApplication loc t1 t2) [t1, t2]
      pure (ms1 <> ms2)
    ELiteral{} ->
      pure []
    EMatch loc t e cs -> do
      ms1 <- collectTypeConstraints e
      (ts1, ts2, ms2) <- collectClauseTypeConstraints (NonEmpty.toList cs)
      -- Pattern types
      assertEquality (RuleMatchClausePatterns loc) (typeOf e : ts1)
      -- Expression types
      assertEquality (RuleMatchClauseExpressions loc) (t : concat ts2)
      pure (ms1 <> ms2)

collectClauseTypeConstraints ::
  (Ord k, Show k, KindRep k) =>
  [Clause Expression a (Type TypeIndex k)] ->
  CollectConstraints (Descriptor k a) k ([Type TypeIndex k], [[Type TypeIndex k]], [Assumption (Type TypeIndex k)])
collectClauseTypeConstraints = third3 concat . unzip3 <$$> traverse go
 where
  go (EClause _ p cs) = do
    (ts1, ms1) <- second concat . unzip <$$> withMonomorphic p $
      forM (NonEmpty.toList cs) $
        \case
          CPlain _ gs e -> do
            ns1 <- concat <$$> forM gs $ \(CGuard g) -> do
              assertEquality RuleMatchClauseGuard [typeOf g, TIntrinsic IBool]
              collectTypeConstraints g
            ns2 <- collectTypeConstraints e
            pure (typeOf e, ns1 <> ns2)
    ms2 <- patternAssumptions ms1 p
    pure (typeOf p, ts1, ms2)

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
