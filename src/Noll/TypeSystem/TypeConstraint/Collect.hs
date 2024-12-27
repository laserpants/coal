{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeConstraint.Collect (
  TypeConstraintsContext (..),
  TypeConstraints (..),
  CollectConstraints,
  collectTypeConstraints,
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

collectTypeConstraints :: (Ord k) => Expression a (Type TypeIndex k) -> CollectConstraints (TypeConstraintMetadata k a) k [Assumption (Type TypeIndex k)]
collectTypeConstraints =
  \case
    EConstructor _ (Label _ name) -> do
      -- TODO
      undefined
    EVariable _ (Label t name) -> do
      pure [Assumption name t]
    ELambda _ ps e -> do
      ms1 <- localMonoset (monosetInsertMany (typeIndexesIn ps)) (collectTypeConstraints e)
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
