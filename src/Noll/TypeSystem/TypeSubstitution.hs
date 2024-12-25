{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeSubstitution (
  TypeSubstitution (..),
  TypeSubstitutable (..),
  mapsToType,
  typeSubstitutionFromList,
) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Expression,
  HasTypeIndexes (..),
  Kind,
  KindIndex (..),
  Pattern,
  Row,
  Scheme (..),
  Trait (..),
  Type,
  TypeIndex (..),
 )
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import qualified Noll.Language.Pattern as Pattern
import qualified Noll.Language.Type as Type
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..))
import Noll.Utils (IndexMap, Map, NonEmpty, Set)

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

instance TypeSubstitutable (Row TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    undefined

instance TypeSubstitutable (MonomorphicSet (TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      MonomorphicSet m ->
        MonomorphicSet (typeIndexesIn (Set.map (apply sub . Type.Variable) m))

instance TypeSubstitutable (Scheme TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      Forall qs ps t ->
        let
          sub1 = foldr removeTypeSubstitution sub qs
         in
          Forall qs (apply sub1 ps) (apply sub1 t)

instance TypeSubstitutable (TypeConstraint TypeIndex (Kind KindIndex) (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      Equality t1 t2 ->
        Equality (apply sub t1) (apply sub t2)
      Implicit t1 t2 m ->
        Implicit (apply sub t1) (apply sub t2) (apply sub m)
      Explicit t1 s ->
        Explicit (apply sub t1) (apply sub s)

instance TypeSubstitutable (Type TypeIndex (Kind KindIndex)) where
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
        fromMaybe (Type.Variable t) (substitutionIndex t sub)
      t@Type.Constructor{} ->
        t

{-# INLINE substitutionIndex #-}
substitutionIndex :: TypeIndex (Kind KindIndex) -> TypeSubstitution -> Maybe (Type TypeIndex (Kind KindIndex))
substitutionIndex TypeIndex{..} sub = Map.lookup indexId (typeSubstitutionMap sub)

instance TypeSubstitutable (Pattern (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      Pattern.Variable (Label t name) ->
        Pattern.Variable (Label (apply sub t) name)

instance TypeSubstitutable (Binding Expression (Type TypeIndex (Kind KindIndex))) where
  apply sub =
    \case
      Binding.Pattern p e ->
        Binding.Pattern (apply sub p) (apply sub e)

instance TypeSubstitutable (Expression (Type TypeIndex (Kind KindIndex))) where
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

newtype TypeSubstitution = TypeSubstitution {typeSubstitutionMap :: IndexMap (Type TypeIndex (Kind KindIndex))}
  deriving (Show, Eq, Ord, Read)

instance Semigroup TypeSubstitution where
  s1 <> s2 = TypeSubstitution (s3 <> typeSubstitutionMap s1)
   where
    s3 = apply s1 (typeSubstitutionMap s2)

instance Monoid TypeSubstitution where
  mempty = TypeSubstitution mempty

{-# INLINE mapsToType #-}
mapsToType :: Int -> Type TypeIndex (Kind KindIndex) -> TypeSubstitution
mapsToType index = TypeSubstitution . Map.singleton index

{-# INLINE removeTypeSubstitution #-}
removeTypeSubstitution :: TypeIndex (Kind KindIndex) -> TypeSubstitution -> TypeSubstitution
removeTypeSubstitution TypeIndex{..} (TypeSubstitution sub) = TypeSubstitution (Map.delete indexId sub)

{-# INLINE typeSubstitutionFromList #-}
typeSubstitutionFromList :: [(Int, Type TypeIndex (Kind KindIndex))] -> TypeSubstitution
typeSubstitutionFromList = TypeSubstitution . Map.fromList
