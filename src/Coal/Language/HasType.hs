{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.HasType (HasType (..), foldTypeOf) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (NonEmpty ((:|)))
import Coal.Language.Expression (Expression (..))
import Coal.Language.Expression.Choice (Guard (..))
import Coal.Language.Module.Constant (Constant (..))
import Coal.Language.Module.Definition (Definition (..))
import Coal.Language.Module.Function (Function (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Primitive (Primitive (..))
import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Type (..), foldType)
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..))
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)

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
      LBignum{} ->
        TIntrinsic IBignum

instance (Data a, Data k, Data (o k), Typeable o) => HasType o k (Pattern a (Type o k)) where
  typeOf =
    \case
      PLiteral _ t ->
        typeOf t
      p ->
        head (universeBi p)

instance (Data a, Data k, Data (o k), Typeable o) => HasType o k (Guard Expression a (Type o k)) where
  typeOf = head . universeBi

instance (HasType o k t) => HasType o k (Label t) where
  typeOf (Label t _) =
    typeOf t

instance (Data a, Data k, Data (o k), Typeable o) => HasType o k (Expression a (Type o k)) where
  typeOf =
    \case
      ELiteral _ t ->
        typeOf t
      ELambda _ ts t ->
        foldTypeOf t ts
      ELet _ _ t ->
        typeOf t
      EFocus _ _ _ _ t ->
        typeOf t
      ESelect _ t _ ->
        typeOf t
      EAnnotation _ _ t ->
        typeOf t
      e ->
        head (universeBi e)

instance (Data a, Data k, Ord k, Data (o k), Typeable o) => HasType o k (Definition a k (Type o k)) where
  typeOf =
    \case
      DAnnotation _ d ->
        typeOf d
      DFunction _ (Function _ _ ps e) _ ->
        foldTypeOf e ps
      DConstant _ (Constant _ _ e) _ ->
        typeOf e
      d ->
        head (universeBi d)

instance HasType o Kind (Trait (Type o Kind)) where
  typeOf (Trait name t) =
    TApplication KTrait (TConstructor (KType `KArrow` KTrait) name) (t :| [])

{-# INLINE foldTypeOf #-}
foldTypeOf :: (HasType o k t, HasType o k s, Functor f, Foldable f) => s -> f t -> Type o k
foldTypeOf a as = foldType (typeOf a) (typeOf <$> as)
