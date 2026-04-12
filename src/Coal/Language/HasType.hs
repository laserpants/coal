{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.HasType (HasType (..), foldTypeOf) where

import Coal.Common.Label (Label (..))
import Coal.Language.Definition
import Coal.Language.Expression (Expression (..))
import Coal.Language.Expression.Choice (Guard (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Primitive (Primitive (..))
import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (KindProxy (..), Type (..), applyTypeArgs, foldType)
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..))
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty ((:|)))

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

instance (Data a, Data s, Data k, Data (o k), Typeable o, Ord k) => HasType o k (Pattern a s (Type o k)) where
  typeOf =
    \case
      PLiteral _ t ->
        typeOf t
      PInteger _ t _ ->
        t
      p ->
        head (universeBi p)

instance (Data a, Data s, Data k, Data (o k), Typeable o) => HasType o k (Guard Expression a s (Type o k)) where
  typeOf = head . universeBi

instance (HasType o k t) => HasType o k (Label t) where
  typeOf (Label t _) =
    typeOf t

instance (Data a, Data s, Data k, Data (o k), Typeable o, Ord k) => HasType o k (Expression a s (Type o k)) where
  typeOf =
    \case
      ELiteral _ t ->
        typeOf t
      ELambda _ ts t ->
        foldTypeOf t ts
      ELet _ _ t ->
        typeOf t
      ERecursiveLet _ _ _ t ->
        typeOf t
      EFocus _ _ _ _ _ t ->
        typeOf t
      ESelect _ t _ ->
        typeOf t
      EAnnotation _ _ t ->
        typeOf t
      EFFICall _ t _ _ _ ->
        typeOf t
      e ->
        head (universeBi e)

instance (Data a, Data k, Data (o k), Typeable o, Ord k) => HasType o k (Definition a k (Type o k)) where
  typeOf =
    \case
      DFunction _ _ FunctionDefinition{..} ->
        foldTypeOf functionDefinitionExpression functionDefinitionPatterns
      DLet _ _ LetDefinition{..} ->
        typeOf letDefinitionExpression
      d ->
        head (universeBi d)

instance (Data (o Kind), Typeable o) => HasType o Kind (Trait (Type o Kind)) where
  typeOf (Trait name t) =
    applyTypeArgs KTrait (TConstructor (KType `KArrow` KTrait) name) (t :| [])

{-# INLINE foldTypeOf #-}
foldTypeOf :: (HasType o k t, HasType o k s, Functor f, Foldable f) => s -> f t -> Type o k
foldTypeOf a as = foldType (typeOf a) (typeOf <$> as)
