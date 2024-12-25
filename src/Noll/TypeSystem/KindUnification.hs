{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindUnification (KindUnifiable (..)) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Noll.Language.Type.Kind (Kind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.Language.Type.Kind.Index (KindIndex (..))
import Noll.TypeSystem.KindSubstitution (KindSubstitutable (..), KindSubstitution (..), applyKindSub, mapsToKind)

class KindUnifiable u where
  unifyKinds :: (Monad m) => u -> u -> m KindSubstitution

instance (KindSubstitutable u, KindUnifiable u) => KindUnifiable [u] where
  unifyKinds [] [] =
    pure mempty
  unifyKinds (u1 : us1) (u2 : us2) = do
    sub1 <- unifyKinds u1 u2
    sub2 <- unifyKinds (applyKindSub sub1 us1) (applyKindSub sub1 us2)
    pure (sub2 <> sub1)
  unifyKinds _ _ =
    error "Implementation error"

instance (KindSubstitutable u, KindUnifiable u) => KindUnifiable (NonEmpty u) where
  unifyKinds u1 u2 = unifyKinds (NonEmpty.toList u1) (NonEmpty.toList u2)

instance KindUnifiable (Kind KindIndex) where
  unifyKinds (Kind.Variable k) k2 =
    pure (bindKind k k2)
  unifyKinds k1 (Kind.Variable k) =
    pure (bindKind k k1)
  unifyKinds Kind.Type Kind.Type =
    pure mempty
  unifyKinds Kind.Row Kind.Row =
    pure mempty
  unifyKinds (Kind.Arrow k1 m1) (Kind.Arrow k2 m2) =
    unifyKinds [k1, m1] [k2, m2]
  unifyKinds _ _ =
    error "Cannot unify" -- unificationError Error.CannotUnify

bindKind :: KindIndex -> Kind KindIndex -> KindSubstitution
bindKind (KindIndex index) =
  \case
    Kind.Variable (KindIndex index2)
      | index2 == index ->
          mempty
    kind ->
      index `mapsToKind` kind
