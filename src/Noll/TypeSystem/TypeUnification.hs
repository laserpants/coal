{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeUnification (TypeUnifiable (..)) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (member)
import Noll.Language.HasTypeIndexes (typeIdsIn)
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Intrinsic (Intrinsic)
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.Language.Type.Kind (Kind)
import Noll.Language.Type.Kind.Index (KindIndex (..))
import Noll.Language.Type.Row (Row (..))
import Noll.TypeSystem.TypeSubstitution (TypeSubstitutable (..), TypeSubstitution (..), mapsTo)

class TypeUnifiable u where
  unify :: (Monad m) => u -> u -> m TypeSubstitution

instance (TypeSubstitutable u, TypeUnifiable u) => TypeUnifiable [u] where
  unify [] [] =
    pure mempty
  unify (u1 : us1) (u2 : us2) = do
    sub1 <- unify u1 u2
    sub2 <- unify (apply sub1 us1) (apply sub1 us2)
    pure (sub2 <> sub1)
  unify _ _ =
    error "Implementation error"

instance (TypeSubstitutable u, TypeUnifiable u) => TypeUnifiable (NonEmpty u) where
  unify u1 u2 = unify (NonEmpty.toList u1) (NonEmpty.toList u2)

instance TypeUnifiable (Row TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))) where
  unify =
    undefined

instance TypeUnifiable (Type TypeIndex (Kind KindIndex)) where
  unify (Type.Alias _ _ t1) t2 =
    unify t1 t2
  unify t1 (Type.Alias _ _ t2) =
    unify t1 t2
  unify (Type.Variable t) t2 =
    bindType t t2
  unify t1 (Type.Variable t) =
    bindType t t1
  unify (Type.Arrow t1 u1) (Type.Arrow t2 u2) =
    unify [t1, u1] [t2, u2]
  unify (Type.Application _ t1 ts1) (Type.Application _ t2 ts2) =
    unify (t1 : NonEmpty.toList ts1) (t2 : NonEmpty.toList ts2)
  unify (Type.Constructor _ c1) (Type.Constructor _ c2)
    | c1 == c2 =
        pure mempty
  unify (Type.Row r1) (Type.Row r2) =
    unify r1 r2
  unify (Type.Intrinsic t1) (Type.Intrinsic t2) =
    unify t1 t2
  unify _ _ =
    error "Cannot unify"

instance TypeUnifiable (Intrinsic (Type TypeIndex (Kind KindIndex))) where
  unify (Intrinsic.List t1) (Intrinsic.List t2) =
    unify t1 t2
  unify (Intrinsic.Option t1) (Intrinsic.Option t2) =
    unify t1 t2
  unify (Intrinsic.Record t1) (Intrinsic.Record t2) =
    unify t1 t2
  unify (Intrinsic.Result t1) (Intrinsic.Result t2) =
    unify t1 t2
  unify (Intrinsic.Tuple ts1) (Intrinsic.Tuple ts2) =
    unify ts1 ts2
  unify t1 t2
    | t1 == t2 =
        pure mempty
  unify _ _ =
    error "Cannot unify"

bindType :: (Monad m) => TypeIndex (Kind KindIndex) -> Type TypeIndex (Kind KindIndex) -> m TypeSubstitution
bindType (TypeIndex _ index) =
  \case
    Type.Variable (TypeIndex _ index2)
      | index == index2 ->
          pure mempty
    t
      | index `member` typeIdsIn t ->
          error "Infinite type"
      | otherwise ->
          pure (index `mapsTo` t)
