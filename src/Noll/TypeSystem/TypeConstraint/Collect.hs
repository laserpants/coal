{-# LANGUAGE FlexibleContexts #-}
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
import Data.List (partition)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra (second)
import Debug.Trace
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  HasType (..),
  HasTypeIndexes (..),
  Intrinsic (..),
  Pattern (..),
  Type (..),
  TypeId (..),
  TypeIndex (..),
  foldType,
 )
import Noll.Language.Type.Scheme (Scheme (..))
import Noll.Library.Environment (Environment (..))
import qualified Noll.Library.Environment as Environment
import Noll.Library.Supply (supplyN)
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..), TypeConstraintMetadata (..), overMonomorphicSet)
import Noll.TypeSystem.TypeConstraint.Assumption (Assumption (..), assumptionNameIs)
import Noll.Utils (Name, concatMapM, forM, forM_, (<$$>))

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
  (Ord k, HasTypeIndexes k s) =>
  s ->
  TypeConstraints c TypeIndex k t a ->
  TypeConstraints c TypeIndex k t a
withMonomorphic ps = localMonoset (monosetInsertMany (typeIndexesIn ps))

collectTypeConstraints ::
  (Ord k, Show k) =>
  Expression a (Type TypeIndex k) ->
  CollectConstraints (TypeConstraintMetadata k a) k [Assumption (Type TypeIndex k)]
collectTypeConstraints =
  \case
    EAnnotation a e -> do
      --      tell [Explicit TypeConstraintMetadata (typeOf e) (annotationScheme a)]
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
    EApplication _ t e1 es -> do
      ms1 <- collectTypeConstraints e1
      ms2 <- concat <$> traverse collectTypeConstraints es
      -- TODO
      assertEquality TypeConstraintMetadata [typeOf e1, foldType t (typeOf <$> es)]
      pure (ms1 <> ms2)
    ELiteral{} ->
      pure []
    EMatch _ t es cs -> do
      ms1 <- concat <$> traverse collectTypeConstraints (NonEmpty.toList es)
      let ts = NonEmpty.toList (typeOf <$> es)
      (ts1, ms2) <-
        second concat . unzip
          <$> forM
            (NonEmpty.toList cs)
            (collectClauseTypeConstraints ts)
      assertEquality TypeConstraintMetadata (t : concat ts1)
      pure (ms1 <> ms2)

collectClauseTypeConstraints ::
  (Ord k, Show k) =>
  [Type TypeIndex k] ->
  Clause Expression a (Type TypeIndex k) ->
  CollectConstraints (TypeConstraintMetadata k a) k ([Type TypeIndex k], [Assumption (Type TypeIndex k)])
collectClauseTypeConstraints ys (EClause _ ps cs) = do
  (ts, ms1) <- second concat . unzip <$> withMonomorphic ps (traverse go (NonEmpty.toList cs))
  forM_ (zip ys (NonEmpty.toList ps)) $
    \case
      (_, PVariable _ (Label t name)) -> do
        undefined
      (t1, PConstructor _ (Label t name) qs) -> do
        r <- lookupContextConstructor name
        case r of
          Nothing ->
            error ("No constructor '" <> Text.unpack name <> "'")
          Just Constructor{..}
            | constructorArity /= length qs ->
                error ("Constructor arity mismatch")
          Just Constructor{..} -> do
            assertEquality TypeConstraintMetadata [t, foldType t1 (typeOf <$> qs)]
            tell [Explicit TypeConstraintMetadata t constructorScheme]
        pure []
  pure (ts, ms1)
 where
  go =
    \case
      CPlain _ gs e -> do
        forM gs $ \g ->
          assertEquality ConstraintClauseGuard [typeOf g, TIntrinsic IBool]
        ms1 <- collectTypeConstraints e
        pure (typeOf e, ms1)

annotationScheme :: Type TypeId () -> Scheme TypeIndex k t
annotationScheme = undefined

box :: (Monad m) => Type TypeId () -> m (Type TypeIndex (Type TypeIndex k))
box =
  \case
    TApplication k t ts ->
      TApplication undefined <$> box t <*> traverse box ts
    TArrow t1 t2 ->
      undefined
    TConstructor{} ->
      undefined
    TIntrinsic{} ->
      undefined
    TRow row ->
      undefined
    TVariable (TypeId _ name) ->
      undefined
    TAlias _ _ t ->
      undefined
