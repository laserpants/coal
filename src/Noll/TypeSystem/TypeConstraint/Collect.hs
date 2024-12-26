{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeConstraint.Collect (
  TypeConstraintsContext (..),
  TypeConstraints (..),
  CollectConstraints,
  collectConstraints,
  runCollectTypeConstraints,
  evalCollectTypeConstraints,
)
where

import Control.Monad.RWS (MonadRWS, MonadReader, MonadState, MonadWriter, RWS, asks, evalRWS, local, tell)
import Data.List (partition)
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Constructor (..),
  Expression (..),
  HasType (..),
  HasTypeIndexes (..),
  Intrinsic (..),
  Pattern (..),
  Type (..),
  TypeIndex (..),
  foldType,
 )
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..), TypeConstraintMetadata (..), overMonomorphicSet)
import Noll.TypeSystem.TypeConstraint.Assumption (Assumption (..), assumptionNameIs)
import Noll.Utils (Dictionary, concatMapM, forM, (<$$>))

data TypeConstraintsContext o k = TypeConstraintsContext
  { contextMonomorphicSet :: MonomorphicSet (o k)
  , contextConstructors :: Dictionary (Constructor o k (Type o k))
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overContextMonomorphicSet #-}
overContextMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> TypeConstraintsContext o k -> TypeConstraintsContext o k
overContextMonomorphicSet fn TypeConstraintsContext{..} = TypeConstraintsContext{contextMonomorphicSet = fn contextMonomorphicSet, ..}

type TypeConstraintsMonad c o k t = RWS (TypeConstraintsContext o k) [TypeConstraint c o k t] ()

newtype TypeConstraints c o k t a = TypeConstraints {constraintsMonad :: TypeConstraintsMonad c o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (TypeConstraintsContext o k)
    , MonadWriter [TypeConstraint c o k t]
    , MonadState ()
    , MonadRWS (TypeConstraintsContext o k) [TypeConstraint c o k t] ()
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

type CollectConstraints c k = TypeConstraints c TypeIndex k (Type TypeIndex k)

{-# INLINE runCollectTypeConstraints #-}
runCollectTypeConstraints :: TypeConstraintsContext o k -> TypeConstraints c o k t a -> (a, [TypeConstraint c o k t])
runCollectTypeConstraints cc cs = evalRWS (constraintsMonad cs) cc ()

{-# INLINE evalCollectTypeConstraints #-}
evalCollectTypeConstraints :: TypeConstraintsContext o k -> TypeConstraints c o k t a -> [TypeConstraint c o k t]
evalCollectTypeConstraints = snd <$$> runCollectTypeConstraints

{-# INLINE assertEquality #-}
assertEquality :: Type TypeIndex k -> Type TypeIndex k -> CollectConstraints TypeConstraintMetadata k ()
assertEquality t1 t2 = tell [Equality TypeConstraintMetadata t1 t2]

assertEqualityAssumptions :: Type TypeIndex k -> [Assumption (Type TypeIndex k)] -> CollectConstraints TypeConstraintMetadata k ()
assertEqualityAssumptions t ms =
  tell $ do
    Assumption{..} <- ms
    pure (Equality TypeConstraintMetadata assumptionType t)

assertImplicitAssumptions :: Type TypeIndex k -> [Assumption (Type TypeIndex k)] -> CollectConstraints TypeConstraintMetadata k ()
assertImplicitAssumptions t ms = do
  set <- asks contextMonomorphicSet
  tell $ do
    Assumption{..} <- ms
    pure (Implicit TypeConstraintMetadata assumptionType t set)

patternAssumptions :: [Assumption (Type TypeIndex k)] -> Pattern a (Type TypeIndex k) -> CollectConstraints TypeConstraintMetadata k [Assumption (Type TypeIndex k)]
patternAssumptions ms =
  \case
    PVariable _ (Label t name) -> do
      let (ls, rs) = partition (assumptionNameIs name) ms
      assertEqualityAssumptions t ls
      pure rs

collectConstraints :: (Ord k) => Expression a (Type TypeIndex k) -> CollectConstraints TypeConstraintMetadata k [Assumption (Type TypeIndex k)]
collectConstraints =
  \case
    EConstructor _ (Label _ name) -> do
      -- TODO
      undefined
    EVariable _ (Label t name) -> do
      pure [Assumption name t]
    ELambda _ ps e -> do
      ms1 <- localMonoset (monosetInsertMany (typeIndexesIn ps)) (collectConstraints e)
      ms2 <- concat <$> forM ps (patternAssumptions ms1)
      pure ms2
    ELet _ gs e1 -> do
      ms1 <- collectConstraints e1
      ms2 <- flip concatMapM gs $
        \case
          BPattern (PVariable _ (Label t name)) e -> do
            ms <- collectConstraints e
            assertEquality t (typeOf e)
            pure ms
      ms3 <- flip concatMapM gs $
        \case
          BPattern (PVariable _ (Label t name)) e -> do
            let (ls, rs) = partition (assumptionNameIs name) ms1
            assertImplicitAssumptions t ls
            pure rs
      pure (ms1 <> ms2 <> ms3)
    EIf _ e1 e2 e3 -> do
      ms1 <- collectConstraints e1
      ms2 <- collectConstraints e2
      ms3 <- collectConstraints e3
      assertEquality (typeOf e1) (TIntrinsic IBool)
      assertEquality (typeOf e2) (typeOf e3)
      pure (ms1 <> ms2 <> ms3)
    EApplication _ t e1 es -> do
      ms1 <- collectConstraints e1
      ms2 <- concat <$> traverse collectConstraints es
      assertEquality (typeOf e1) (foldType t (typeOf <$> es))
      pure (ms1 <> ms2)
    ELiteral{} ->
      pure []
