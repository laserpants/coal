{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindUnification (KindUnifiable (unifyKinds)) where

import Control.Monad.Except
import qualified Data.List.NonEmpty as NonEmpty
import Noll.Language (Kind (..), KindIndex (..))
import Noll.TypeSystem.KindSubstitution (KindSubstitutable (..), KindSubstitution (..), applyKindSub, mapsToKind)
import Noll.TypeSystem.Unification.Error (UnificationError (..))
import qualified Noll.TypeSystem.Unification.Error as Error
import Noll.Utils (NonEmpty)

class KindUnifiable u where
  unifyKinds :: (MonadError UnificationError m) => u -> u -> m KindSubstitution

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
  unifyKinds (KVariable k) k2 =
    pure (bindKind k k2)
  unifyKinds k1 (KVariable k) =
    pure (bindKind k k1)
  unifyKinds KType KType =
    pure mempty
  unifyKinds KRow KRow =
    pure mempty
  unifyKinds (KArrow k1 m1) (KArrow k2 m2) =
    unifyKinds [k1, m1] [k2, m2]
  unifyKinds _ _ =
    throwError Error.CannotUnifyKinds

bindKind :: KindIndex -> Kind KindIndex -> KindSubstitution
bindKind (KindIndex index) =
  \case
    KVariable (KindIndex index2)
      | index2 == index ->
          mempty
    kind ->
      index `mapsToKind` kind
