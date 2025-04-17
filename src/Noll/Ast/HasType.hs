{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Ast.HasType (HasType (..), foldTypeOf) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Noll.Language.Expression (Expression (..))
import Noll.Language.Expression.Choice (Guard (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Language.Type (Type (..), foldType)
import Noll.Language.Type.Intrinsic (Intrinsic (..))
import Noll.Module.Constant (Constant (..))
import Noll.Module.Definition (Definition (..))
import Noll.Module.Function (Function (..))

-- TODO: move?
class HasType o k t where
  typeOf :: t -> Type o k

instance HasType o k (Type o k) where
  typeOf = id

instance HasType o k Primitive where
  typeOf =
    \case
      LUnit ->
        TIntrinsic IUnit
      LBool{} ->
        TIntrinsic IBool
      LInt32{} ->
        TIntrinsic IInt32
      LInt64{} ->
        TIntrinsic IInt64
      LFloat{} ->
        TIntrinsic IFloat
      LDouble{} ->
        TIntrinsic IDouble
      LChar{} ->
        TIntrinsic IChar
      LString{} ->
        TIntrinsic IString

instance (Data a, Data k, Data (o k), Typeable o) => HasType o k (Pattern a (Type o k)) where
  typeOf =
    \case
      PLiteral _ t ->
        typeOf t
      p ->
        head (universeBi p)

instance (Data a, Data k, Data (o k), Typeable o) => HasType o k (Guard Expression a (Type o k)) where
  typeOf = head . universeBi

instance (Data a, Data k, Data (o k), Typeable o) => HasType o k (Expression a (Type o k)) where
  typeOf =
    \case
      ELiteral _ t ->
        typeOf t
      ELambda _ ts t ->
        foldTypeOf t ts
      ELet _ _ t ->
        typeOf t
      e ->
        head (universeBi e)

instance (Data a, Data k, Ord k, Data (o k), Typeable o) => HasType o k (Definition a k (Type o k)) where
  typeOf =
    \case
      DAnnotation _ d ->
        typeOf d
      DFunction _ (Function _ _ ps e) ->
        foldTypeOf e ps
      DConstant _ (Constant _ _ e) ->
        typeOf e
      d ->
        head (universeBi d)

{-# INLINE foldTypeOf #-}
foldTypeOf :: (HasType o k t, HasType o k s, Functor f, Foldable f) => s -> f t -> Type o k
foldTypeOf a as = foldType (typeOf a) (typeOf <$> as)
