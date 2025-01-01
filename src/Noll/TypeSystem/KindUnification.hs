{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.KindUnification (KindUnifiable (unify)) where

import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Noll.Language (Kind (..), KindIndex (..))
import Noll.TypeSystem.KindSubstitution (KindSubstitutable (..), KindSubstitution (..), apply, mapsToKind)
import Noll.TypeSystem.Unification.Error (UnificationError (..))
import qualified Noll.TypeSystem.Unification.Error as Error

class KindUnifiable u where
  unify :: (MonadError UnificationError m) => u -> u -> m KindSubstitution

instance (KindSubstitutable u, KindUnifiable u) => KindUnifiable [u] where
  unify [] [] =
    pure mempty
  unify (u1 : us1) (u2 : us2) = do
    sub1 <- unify u1 u2
    sub2 <- unify (apply sub1 us1) (apply sub1 us2)
    pure (sub2 <> sub1)
  unify _ _ =
    error "Implementation error"

instance (KindSubstitutable u, KindUnifiable u) => KindUnifiable (NonEmpty u) where
  unify u1 u2 = unify (NonEmpty.toList u1) (NonEmpty.toList u2)

instance KindUnifiable (Kind KindIndex) where
  unify (KVariable k) k2 =
    pure (bindKind k k2)
  unify k1 (KVariable k) =
    pure (bindKind k k1)
  unify KType KType =
    pure mempty
  unify KRow KRow =
    pure mempty
  unify (KArrow k1 m1) (KArrow k2 m2) =
    unify [k1, m1] [k2, m2]
  unify _ _ =
    throwError Error.CannotUnifyKinds

bindKind :: KindIndex -> Kind KindIndex -> KindSubstitution
bindKind (KindIndex index) =
  \case
    KVariable (KindIndex index2)
      | index2 == index ->
          mempty
    kind ->
      index `mapsToKind` kind
