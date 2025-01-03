{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Substitution (
  Substitutable (..),
  Substitution (..),
  mapsTo,
  substitutionFromList,
) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Expression (..),
  Guard (..),
  Intrinsic (..),
  Kind,
  Pattern (..),
  Row (..),
  Scheme (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
 )
import Noll.TypeSystem.Constraint (Constraint (..), MonomorphicSet (..))
import Noll.Utils (IndexMap, Map, Set, fromMaybe)

class Substitutable s where
  apply :: Substitution -> s -> s

instance (Substitutable s) => Substitutable (Map k s) where
  apply = fmap . apply

instance (Substitutable s) => Substitutable [s] where
  apply = fmap . apply

instance (Substitutable s) => Substitutable (NonEmpty s) where
  apply = fmap . apply

instance (Substitutable s) => Substitutable (Maybe s) where
  apply = fmap . apply

instance (Substitutable s) => Substitutable (Trait s) where
  apply = fmap . apply

instance (Ord s, Substitutable s) => Substitutable (Set s) where
  apply = Set.map . apply

instance Substitutable (MonomorphicSet (TypeIndex Kind)) where
  apply sub =
    \case
      MonomorphicSet m ->
        MonomorphicSet (typeIndexesIn (Set.map (apply sub . TVariable) m))

instance Substitutable (Scheme TypeIndex Kind (Type TypeIndex Kind)) where
  apply sub =
    \case
      Forall qs ps t ->
        let
          sub1 = foldr removeSubstitution sub qs
         in
          Forall qs (apply sub1 ps) (apply sub1 t)

instance Substitutable (Constraint c TypeIndex Kind (Type TypeIndex Kind)) where
  apply sub =
    \case
      Equality c ts ->
        Equality c (apply sub ts)
      Implicit c t1 t2 m ->
        Implicit c (apply sub t1) (apply sub t2) (apply sub m)
      Explicit c t1 s ->
        Explicit c (apply sub t1) (apply sub s)

instance (Substitutable s) => Substitutable (Intrinsic s) where
  apply = fmap . apply

instance Substitutable (Row TypeIndex Kind (Type TypeIndex Kind)) where
  apply sub =
    error "TODO"

instance Substitutable (Type TypeIndex Kind) where
  apply sub =
    \case
      TAlias name ts t -> do
        TAlias name (apply sub ts) (apply sub t)
      TApplication k t1 ts ->
        TApplication k (apply sub t1) (apply sub ts)
      TArrow t1 t2 ->
        TArrow (apply sub t1) (apply sub t2)
      TIntrinsic t ->
        TIntrinsic (apply sub t)
      TRow row ->
        TRow (apply sub row)
      TVariable t ->
        fromMaybe (TVariable t) (substitutionIndex t sub)
      t@TConstructor{} ->
        t

instance Substitutable (Pattern a (Type TypeIndex Kind)) where
  apply sub =
    \case
      PVariable a (Label t name) ->
        PVariable a (Label (apply sub t) name)
      PConstructor a (Label t name) ps ->
        PConstructor a (Label (apply sub t) name) (apply sub ps)

instance Substitutable (Binding Expression a (Type TypeIndex Kind)) where
  apply sub =
    \case
      BPattern a p e ->
        BPattern a (apply sub p) (apply sub e)

instance Substitutable (Guard Expression a (Type TypeIndex Kind)) where
  apply sub =
    \case
      CGuard e ->
        CGuard (apply sub e)

instance Substitutable (Choice Expression a (Type TypeIndex Kind)) where
  apply sub =
    \case
      CPlain a gs e ->
        CPlain a (apply sub gs) (apply sub e)

instance Substitutable (Clause Expression a (Type TypeIndex Kind)) where
  apply sub =
    \case
      EClause a p cs ->
        EClause a (apply sub p) (apply sub cs)

instance Substitutable (Expression a (Type TypeIndex Kind)) where
  apply sub =
    \case
      EAnnotation a t e ->
        EAnnotation a t (apply sub e)
      EConstructor a (Label t name) -> do
        EConstructor a (Label (apply sub t) name)
      EVariable a (Label t name) -> do
        EVariable a (Label (apply sub t) name)
      ELambda a ps e -> do
        ELambda a (apply sub ps) (apply sub e)
      ELet a gs e1 -> do
        ELet a (apply sub gs) (apply sub e1)
      EIf a t e1 e2 e3 -> do
        EIf a (apply sub t) (apply sub e1) (apply sub e2) (apply sub e3)
      EApplication a t e1 es -> do
        EApplication a (apply sub t) (apply sub e1) (apply sub es)
      EMatch a t e cs ->
        EMatch a (apply sub t) (apply sub e) (apply sub cs)
      e@ELiteral{} ->
        e

newtype Substitution = Substitution {substitutionMap :: IndexMap (Type TypeIndex Kind)}
  deriving (Show, Eq, Ord, Read)

instance Semigroup Substitution where
  s1 <> s2 = Substitution (s3 <> substitutionMap s1)
   where
    s3 = apply s1 (substitutionMap s2)

instance Monoid Substitution where
  mempty = Substitution mempty

{-# INLINE substitutionIndex #-}
substitutionIndex :: TypeIndex Kind -> Substitution -> Maybe (Type TypeIndex Kind)
substitutionIndex TypeIndex{..} sub = Map.lookup typeIndexId (substitutionMap sub)

{-# INLINE removeSubstitution #-}
removeSubstitution :: TypeIndex Kind -> Substitution -> Substitution
removeSubstitution TypeIndex{..} (Substitution sub) = Substitution (Map.delete typeIndexId sub)

{-# INLINE mapsTo #-}
mapsTo :: Int -> Type TypeIndex Kind -> Substitution
mapsTo index = Substitution . Map.singleton index

{-# INLINE substitutionFromList #-}
substitutionFromList :: [(Int, Type TypeIndex Kind)] -> Substitution
substitutionFromList = Substitution . Map.fromList
