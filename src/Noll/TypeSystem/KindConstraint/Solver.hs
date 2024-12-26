{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindConstraint.Solver (solveKinds) where

import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.TypeSystem.KindSubstitution (KindSubstitutable (..), KindSubstitution (..))
import Noll.TypeSystem.KindUnification (KindUnifiable (..))

solveKinds ::
  ( KindSubstitutable k
  , KindUnifiable k
  , Monad m
  ) =>
  [KindConstraint c k] ->
  m KindSubstitution
solveKinds [] =
  pure mempty
solveKinds (KindEquality _ k1 k2 : cs) = do
  sub1 <- unifyKinds k1 k2
  sub2 <- solveKinds (applyKindSub sub1 cs)
  pure (sub2 <> sub1)
