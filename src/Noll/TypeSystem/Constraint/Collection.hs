{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Collection (
  ConstraintsContext (..),
  Constraints (..),
)
where

import Control.Monad.RWS (MonadRWS, MonadReader, MonadState, MonadWriter, RWS)
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import qualified Noll.Language.Expression as Expr
import Noll.Language.HasType (HasType (..))
import Noll.Language.Type (Type (..))
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Opaque (OpaqueType)
import Noll.TypeSystem.Assumption (Assumption (..))
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.Utils (Some)

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

assertEquality = undefined

foldType :: OpaqueType -> Some OpaqueType -> OpaqueType
foldType = undefined

collectConstraints ::
  Expression OpaqueType ->
  Constraints TypeIndex () OpaqueType ([Assumption OpaqueType], Expression OpaqueType)
collectConstraints =
  \case
    Expr.Constructor (Label _ name) -> do
      undefined
    Expr.Variable (Label t name) -> do
      pure ([Assumption name t], Expr.Variable (Label t name))
    Expr.Lambda ps e -> do
      undefined
    Expr.Let ls e1 -> do
      (ms1, a1) <- collectConstraints e1
      undefined
    Expr.If e1 e2 e3 -> do
      undefined
    Expr.Application t e1 es -> do
      (ms1, a1) <- collectConstraints e1
      (ms2, as) <- sequence <$> traverse collectConstraints es
      assertEquality a1 (foldType t (typeOf <$> as))
      pure (ms1 <> ms2, Expr.Application t a1 as)
    lit@Expr.Literal{} ->
      pure ([], lit)
