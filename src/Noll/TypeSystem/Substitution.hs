{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Substitution (
  TypeSubstitution (..),
  TypeSubstitutable (..),
  KindSubstitution (..),
  KindSubstitutable (..),
  mapsTo,
  substitutionFromList,
  mapsToKind,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import qualified Noll.Language.Expression as Expr
import Noll.Language.Expression.Binding (Binding (..))
import qualified Noll.Language.Expression.Binding as Binding
import Noll.Language.HasTypeIndexes (HasTypeIndexes (..))
import Noll.Language.Pattern (Pattern)
import qualified Noll.Language.Pattern as Pattern
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.Language.Type.Row (Row (..))
import Noll.Language.Type.Scheme (Scheme (..))
import Noll.TypeSystem.Constraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.Utils (IndexMap)

class TypeSubstitutable s where
  apply :: TypeSubstitution -> s -> s

instance (TypeSubstitutable s) => TypeSubstitutable (Map k s) where
  apply = fmap . apply

instance (TypeSubstitutable s) => TypeSubstitutable [s] where
  apply = fmap . apply

instance (TypeSubstitutable s) => TypeSubstitutable (NonEmpty s) where
  apply = fmap . apply

instance (TypeSubstitutable s) => TypeSubstitutable (Maybe s) where
  apply = fmap . apply

instance (TypeSubstitutable s) => TypeSubstitutable (Trait s) where
  apply = fmap . apply

instance (Ord s, TypeSubstitutable s) => TypeSubstitutable (Set s) where
  apply = Set.map . apply

instance TypeSubstitutable (Row TypeIndex (Kind Int) (Type TypeIndex (Kind Int))) where
  apply sub =
    undefined

instance TypeSubstitutable (MonomorphicSet (TypeIndex (Kind Int))) where
  apply sub =
    \case
      MonomorphicSet m ->
        MonomorphicSet (typeIndexesIn (Set.map (apply sub . Type.Variable) m))

instance TypeSubstitutable (Scheme TypeIndex (Kind Int) (Type TypeIndex (Kind Int))) where
  apply sub =
    \case
      Forall qs ps t ->
        let
          sub1 = foldr removeSubstitution sub qs
         in
          Forall qs (apply sub1 ps) (apply sub1 t)

instance TypeSubstitutable (TypeConstraint TypeIndex (Kind Int) (Type TypeIndex (Kind Int))) where
  apply sub =
    \case
      Equality t1 t2 ->
        Equality (apply sub t1) (apply sub t2)
      Implicit t1 t2 m ->
        Implicit (apply sub t1) (apply sub t2) (apply sub m)
      Explicit t1 s ->
        Explicit (apply sub t1) (apply sub s)

instance TypeSubstitutable (Type TypeIndex (Kind Int)) where
  apply sub =
    \case
      Type.Alias name ts t -> do
        Type.Alias name (apply sub ts) (apply sub t)
      Type.Application k t1 ts ->
        Type.Application k (apply sub t1) (apply sub ts)
      Type.Arrow t1 t2 ->
        Type.Arrow (apply sub t1) (apply sub t2)
      Type.Intrinsic t ->
        Type.Intrinsic (apply sub <$> t)
      Type.Row row ->
        Type.Row (apply sub row)
      Type.Variable t ->
        fromMaybe (Type.Variable t) (typeSubstitutionIndex t sub)
      t@Type.Constructor{} ->
        t

instance TypeSubstitutable (Pattern (Type TypeIndex (Kind Int))) where
  apply sub =
    \case
      Pattern.Variable (Label t name) ->
        Pattern.Variable (Label (apply sub t) name)

instance TypeSubstitutable (Binding Expression (Type TypeIndex (Kind Int))) where
  apply sub =
    \case
      Binding.Pattern p e ->
        Binding.Pattern (apply sub p) (apply sub e)

instance TypeSubstitutable (Expression (Type TypeIndex (Kind Int))) where
  apply sub =
    \case
      Expr.Constructor (Label t name) -> do
        Expr.Constructor (Label (apply sub t) name)
      Expr.Variable (Label t name) -> do
        Expr.Variable (Label (apply sub t) name)
      Expr.Lambda ps e -> do
        Expr.Lambda (apply sub ps) (apply sub e)
      Expr.Let gs e1 -> do
        Expr.Let (apply sub gs) (apply sub e1)
      Expr.If e1 e2 e3 -> do
        Expr.If (apply sub e1) (apply sub e2) (apply sub e3)
      Expr.Application t e1 es -> do
        Expr.Application (apply sub t) (apply sub e1) (apply sub es)
      e@Expr.Literal{} ->
        e

newtype TypeSubstitution = TypeSubstitution {typeSubstitutionMap :: IndexMap (Type TypeIndex (Kind Int))}
  deriving (Show, Eq, Ord, Read)

instance Semigroup TypeSubstitution where
  s1 <> s2 = TypeSubstitution (s3 <> typeSubstitutionMap s1)
   where
    s3 = apply s1 (typeSubstitutionMap s2)

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution mempty

{-# INLINE typeSubstitutionIndex #-}
typeSubstitutionIndex :: TypeIndex (Kind Int) -> TypeSubstitution -> Maybe (Type TypeIndex (Kind Int))
typeSubstitutionIndex TypeIndex{..} sub = Map.lookup indexId (typeSubstitutionMap sub)

{-# INLINE removeSubstitution #-}
removeSubstitution :: TypeIndex (Kind Int) -> TypeSubstitution -> TypeSubstitution
removeSubstitution TypeIndex{..} (TypeSubstitution sub) = TypeSubstitution (Map.delete indexId sub)

{-# INLINE mapsTo #-}
mapsTo :: Int -> Type TypeIndex (Kind Int) -> TypeSubstitution
mapsTo index = TypeSubstitution . Map.singleton index

{-# INLINE substitutionFromList #-}
substitutionFromList :: [(Int, Type TypeIndex (Kind Int))] -> TypeSubstitution
substitutionFromList = TypeSubstitution . Map.fromList

--

class KindSubstitutable s where
  kindApply :: KindSubstitution -> s -> s

instance (KindSubstitutable s) => KindSubstitutable (Map k s) where
  kindApply = fmap . kindApply

instance (KindSubstitutable s) => KindSubstitutable [s] where
  kindApply = fmap . kindApply

instance (KindSubstitutable s) => KindSubstitutable (NonEmpty s) where
  kindApply = fmap . kindApply

instance (KindSubstitutable s) => KindSubstitutable (Maybe s) where
  kindApply = fmap . kindApply

instance (KindSubstitutable s) => KindSubstitutable (Trait s) where
  kindApply = fmap . kindApply

instance (Ord s, KindSubstitutable s) => KindSubstitutable (Set s) where
  kindApply = Set.map . kindApply

instance KindSubstitutable (Kind Int) where
  kindApply sub =
    \case
      Kind.Arrow k1 k2 ->
        Kind.Arrow (kindApply sub k1) (kindApply sub k2)
      Kind.Variable k ->
        fromMaybe (Kind.Variable k) (kindSubstitutionIndex k sub)
      k ->
        k

instance KindSubstitutable (TypeIndex (Kind Int)) where
  kindApply sub =
    \case
      TypeIndex k index ->
        TypeIndex (kindApply sub k) index

instance KindSubstitutable (Row TypeIndex (Kind Int) (Type TypeIndex (Kind Int))) where
  kindApply sub =
    undefined

instance KindSubstitutable (Type TypeIndex (Kind Int)) where
  kindApply sub =
    \case
      Type.Alias name ts t -> do
        Type.Alias name (kindApply sub ts) (kindApply sub t)
      Type.Application k t1 ts ->
        Type.Application (kindApply sub k) (kindApply sub t1) (kindApply sub ts)
      Type.Arrow t1 t2 ->
        Type.Arrow (kindApply sub t1) (kindApply sub t2)
      Type.Intrinsic t ->
        Type.Intrinsic (kindApply sub <$> t)
      Type.Row row ->
        Type.Row (kindApply sub row)
      Type.Variable t ->
        Type.Variable (kindApply sub t)
      Type.Constructor k name ->
        Type.Constructor (kindApply sub k) name

instance KindSubstitutable (Expression (Type TypeIndex (Kind Int))) where
  kindApply = fmap . kindApply

newtype KindSubstitution = KindSubstitution {kindSubstitutionMap :: IndexMap (Kind Int)}
  deriving (Show, Eq, Ord, Read)

instance Semigroup KindSubstitution where
  s1 <> s2 = KindSubstitution (s3 <> kindSubstitutionMap s1)
   where
    s3 = kindApply s1 (kindSubstitutionMap s2)

instance Monoid KindSubstitution where
  mempty = KindSubstitution mempty

{-# INLINE kindSubstitutionIndex #-}
kindSubstitutionIndex :: Int -> KindSubstitution -> Maybe (Kind Int)
kindSubstitutionIndex index sub = Map.lookup index (kindSubstitutionMap sub)

{-# INLINE mapsToKind #-}
mapsToKind :: Int -> Kind Int -> KindSubstitution
mapsToKind index = KindSubstitution . Map.singleton index
