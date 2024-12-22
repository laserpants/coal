{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Collect (
  ConstraintsContext (..),
  Constraints (..),
)
where

import Control.Monad (forM)
import Control.Monad.RWS (MonadRWS, MonadReader, MonadState, MonadWriter, RWS)
import Data.List (partition)
import qualified Data.List.NonEmpty as NonEmptyList
import qualified Data.Set as Set
import Data.Tuple.Extra (first)
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import Noll.Language.HasType (HasType (..))
import Noll.Language.Pattern (Pattern (..))
import qualified Noll.Language.Pattern as Pattern
import Noll.Language.Type (Type (..), foldType)
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.Language.Type.Opaque (OpaqueType)
import Noll.TypeSystem.Assumption (Assumption (..), assumptionNameIs)
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..), overMonomorphicSet)
import Noll.Utils (Some, (<$$>))

data ConstraintsContext o k = ConstraintsContext
  { contextMonomorphicSet :: MonomorphicSet (o k)
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overContextMonomorphicSet #-}
overContextMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> ConstraintsContext o k -> ConstraintsContext o k
overContextMonomorphicSet fn ConstraintsContext{..} = ConstraintsContext{contextMonomorphicSet = fn contextMonomorphicSet, ..}

type ConstraintsMonad o k t = RWS (ConstraintsContext o k) [TypeConstraint o k t] ()

newtype Constraints o k t a = Constraints {constraintsMonad :: ConstraintsMonad o k t a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ConstraintsContext o k)
    , MonadWriter [TypeConstraint o k t]
    , MonadState ()
    , MonadRWS (ConstraintsContext o k) [TypeConstraint o k t] ()
    )

localMonoset = undefined

monosetInsert :: TypeIndex () -> MonomorphicSet (TypeIndex ()) -> MonomorphicSet (TypeIndex ())
monosetInsert = overMonomorphicSet . Set.insert

monosetInsertMany :: (Foldable f) => f (TypeIndex ()) -> MonomorphicSet (TypeIndex ()) -> MonomorphicSet (TypeIndex ())
monosetInsertMany a b = foldr monosetInsert b a

assertEquality = undefined

assertEqualityAssumptions = undefined

assertImplicitAssumptions = undefined

type CollectConstraints = Constraints TypeIndex () OpaqueType

patternAssumptions :: [Assumption OpaqueType] -> Pattern OpaqueType -> CollectConstraints [Assumption OpaqueType]
patternAssumptions ms =
  \case
    Pattern.Variable (Label t name) -> do
      let (ls, rs) = partition (assumptionNameIs name) ms
      assertEqualityAssumptions t ls
      pure rs

collectConstraints :: Expression OpaqueType -> CollectConstraints ([Assumption OpaqueType], Expression OpaqueType)
collectConstraints =
  \case
    Expr.Constructor (Label _ name) -> do
      undefined
    Expr.Variable (Label t name) -> do
      pure ([Assumption name t], Expr.Variable (Label t name))
    Expr.Lambda ps e -> do
      let ts = undefined :: [TypeIndex ()]
      (ms1, a1) <- localMonoset (monosetInsertMany ts) (collectConstraints e)
      ms2 <- concat <$> forM ps (patternAssumptions ms1)
      pure (ms2, Expr.Lambda ps a1)
    Expr.Let ds e1 -> do
      (ms1, a1) <- collectConstraints e1
      ms2 <- concat <$$> forM ds $
        \case
          Binding.Pattern (Pattern.Variable (Label t name)) e -> do
            (ms, a) <- collectConstraints e
            assertEquality t a
            pure ms
      ms3 <- concat <$$> forM ds $
        \case
          Binding.Pattern (Pattern.Variable (Label t name)) e -> do
            let (ls, rs) = partition (assumptionNameIs name) ms1
            assertImplicitAssumptions t ls
            pure rs
      pure (ms1 <> ms2 <> ms3, Expr.Let ds a1)
    Expr.If e1 e2 e3 -> do
      (ms1, a1) <- collectConstraints e1
      (ms2, a2) <- collectConstraints e2
      (ms3, a3) <- collectConstraints e3
      assertEquality a1 (Intrinsic Intrinsic.Bool)
      assertEquality a2 a3
      pure (ms1 <> ms2 <> ms3, Expr.If a1 a2 a3)
    Expr.Application t e1 es -> do
      (ms1, a1) <- collectConstraints e1
      (ms2, as) <- sequence <$> traverse collectConstraints es
      assertEquality a1 (foldType t (typeOf <$> as))
      pure (ms1 <> ms2, Expr.Application t a1 as)
    lit@Expr.Literal{} ->
      pure ([], lit)
