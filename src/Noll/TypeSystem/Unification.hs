{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Unification (TypeUnifiable (..)) where

import Noll.Language.Type (Type)
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Intrinsic (Intrinsic)
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.Language.Type.Kind (Kind)
import Noll.TypeSystem.Substitution (TypeSubstitutable (..), TypeSubstitution (..))

class TypeUnifiable u where
  unify :: (Monad m) => u -> u -> m TypeSubstitution

instance (TypeSubstitutable u, TypeUnifiable u) => TypeUnifiable [u] where
  unify [] [] = pure mempty
  unify (u1 : us1) (u2 : us2) = do
    sub1 <- unify u1 u2
    sub2 <- unify (apply sub1 us1) (apply sub1 us2)
    pure (sub2 <> sub1)
  unify _ _ = error "Implementation error"

instance TypeUnifiable (Type TypeIndex (Kind Int)) where
  unify =
    undefined

instance TypeUnifiable (Intrinsic (Type TypeIndex (Kind Int))) where
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

--  unify _ _ =
--    unificationError Error.CannotUnify
