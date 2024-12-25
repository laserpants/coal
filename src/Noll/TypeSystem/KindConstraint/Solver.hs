{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint.Solver where

import Control.Monad.State (MonadState)
import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.TypeSystem.KindSubstitution (KindSubstitutable (..), KindSubstitution (..))
import Noll.TypeSystem.KindUnification (KindUnifiable (..))

solveKinds ::
  ( KindSubstitutable (KindConstraint k)
  , KindUnifiable k
  , MonadState Int m
  ) =>
  [KindConstraint k] ->
  m KindSubstitution
solveKinds [] =
  pure mempty
solveKinds (KindEquality k1 k2 : cs) = do
  sub1 <- unifyKinds k1 k2
  sub2 <- solveKinds (applyKindSub sub1 cs)
  pure (sub2 <> sub1)
