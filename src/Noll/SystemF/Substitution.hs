{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Substitution (
  Substitutable (..),
  Substitution (..),
  mapsTo,
  fromList,
  normalizeTypeIndexes,
) where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBi)
import Data.List.NonEmpty (NonEmpty)
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  CompiledClause (..),
  Constant (..),
  Definition (..),
  Expression (..),
  Function (..),
  Guard (..),
  IndexedType,
  Intrinsic (..),
  Kind,
  Pattern (..),
  Row (..),
  Scheme (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
  Uses (..),
 )
import Noll.SystemF.Constraint (Constraint (..), MonomorphicSet (..))
import Noll.Utils (IndexMap, Map, Set, fromMaybe)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

class Substitutable s where
  apply :: Substitution -> s -> s

applyR :: Substitution -> Row TypeIndex Kind IndexedType -> Row TypeIndex Kind IndexedType
applyR sub =
    \case
      RVariable r ->
        case substitutionIndex r sub of
          Just (TRow row) ->
            row
          _ ->
            RVariable r
      r ->
        r 

applyT :: Substitution -> IndexedType -> IndexedType
applyT sub =
    \case
      TRow row ->
        TRow (applyR sub row)
      TVariable t ->
        fromMaybe (TVariable t) (substitutionIndex t sub)
      t ->
        t

instance (Data s, Data k, Ord k) => Substitutable (Map k s) where
  apply = transformBi . applyT

--instance (Substitutable s) => Substitutable [s] where
--  apply = fmap . apply
--
--instance (Substitutable s) => Substitutable (NonEmpty s) where
--  apply = fmap . apply
--
--instance (Substitutable s) => Substitutable (Maybe s) where
--  apply = fmap . apply
--
--instance (Substitutable s) => Substitutable (Trait s) where
--  apply = fmap . apply
--
--instance (Ord s, Substitutable s) => Substitutable (Set s) where
--  apply = Set.map . apply
--
--instance Substitutable (MonomorphicSet (TypeIndex Kind)) where
--  apply sub =
--    \case
--      MonomorphicSet m ->
--        MonomorphicSet (typeIndexesIn (Set.map (apply sub . TVariable) m))
--
--instance Substitutable (Scheme TypeIndex Kind IndexedType) where
--  apply sub =
--    \case
--      Forall qs ps t ->
--        Forall qs (apply sub1 ps) (apply sub1 t)
--       where
--        sub1 = foldr removeSubstitution sub qs
--
--instance Substitutable (Constraint c TypeIndex Kind IndexedType) where
--  apply sub =
--    \case
--      Equality c ts ->
--        Equality c (apply sub ts)
--      Implicit c t1 t2 m ->
--        Implicit c (apply sub t1) (apply sub t2) (apply sub m)
--      Explicit c t1 s ->
--        Explicit c (apply sub t1) (apply sub s)
--
--instance (Substitutable s) => Substitutable (Intrinsic s) where
--  apply = fmap . apply
--
--instance Substitutable (Row TypeIndex Kind IndexedType) where
--  apply sub =
--    \case
--      RExtend name t row ->
--        RExtend name (apply sub t) (apply sub row)
--      RVariable r ->
--        case substitutionIndex r sub of
--          Just (TRow row) ->
--            row
--          _ ->
--            RVariable r
--      RNil ->
--        RNil
--
--instance Substitutable IndexedType where
--  apply sub =
--    \case
--      TAlias name ts t -> do
--        TAlias name (apply sub ts) (apply sub t)
--      TApplication k t1 ts ->
--        TApplication k (apply sub t1) (apply sub ts)
--      TArrow t1 t2 ->
--        TArrow (apply sub t1) (apply sub t2)
--      TIntrinsic t ->
--        TIntrinsic (apply sub t)
--      TRow row ->
--        TRow (apply sub row)
--      TVariable t ->
--        fromMaybe (TVariable t) (substitutionIndex t sub)
--      t@TConstructor{} ->
--        t
--
--instance Substitutable (Pattern a IndexedType) where
--  apply sub =
--    \case
--      PAnnotation a t p ->
--        PAnnotation a t (apply sub p)
--      PVariable a (Label t name) ->
--        PVariable a (Label (apply sub t) name)
--      PConstructor a (Label t name) ps ->
--        PConstructor a (Label (apply sub t) name) (apply sub ps)
--      POr a t p1 p2 ->
--        POr a (apply sub t) (apply sub p1) (apply sub p2)
--      PRecord a t d p ->
--        PRecord a (apply sub t) (apply sub d) (apply sub p)
--      PShorthand a (Label t name) ->
--        PShorthand a (Label (apply sub t) name)
--      PAny a t ->
--        PAny a (apply sub t)
--      PListCons a t p1 p2 ->
--        PListCons a (apply sub t) (apply sub p1) (apply sub p2)
--      PListLiteral a t ps ->
--        PListLiteral a (apply sub t) (apply sub ps)
--      PAtVariable a (Label t name) ->
--        PAtVariable a (Label (apply sub t) name)
--      p@PLiteral{} ->
--        p
--
--instance Substitutable (Binding Expression a IndexedType) where
--  apply sub =
--    \case
--      BPattern a p e ->
--        BPattern a (apply sub p) (apply sub e)
--      BFunction a name ps e ->
--        BFunction a name (apply sub ps) (apply sub e)
--
--instance Substitutable (Guard Expression a IndexedType) where
--  apply sub =
--    \case
--      CGuard e ->
--        CGuard (apply sub e)
--
--instance Substitutable (Choice Expression a IndexedType) where
--  apply sub =
--    \case
--      CPlain a gs e ->
--        CPlain a (apply sub gs) (apply sub e)
--      CLambda{} ->
--        error "TODO"
--
--instance Substitutable (Clause Expression a IndexedType) where
--  apply sub =
--    \case
--      EClause a p cs ->
--        EClause a (apply sub p) (apply sub cs)
--
--instance (Substitutable t) => Substitutable (Label t) where
--  apply sub =
--    \case
--      Label t name ->
--        Label (apply sub t) name
--
--instance Substitutable (CompiledClause Expression a IndexedType) where
--  apply sub =
--    \case
--      ECompiledClause lls e ->
--        ECompiledClause (apply sub lls) (apply sub e)
--      ECompiledField name ll1 ll2 e ->
--        ECompiledField name (apply sub ll1) (apply sub ll2) (apply sub e)
--
--instance Substitutable (Expression a IndexedType) where
--  apply sub =
--    \case
--      EAnnotation a t e ->
--        EAnnotation a t (apply sub e)
--      EConstructor a (Label t name) -> do
--        EConstructor a (Label (apply sub t) name)
--      EVariable a (Label t name) -> do
--        EVariable a (Label (apply sub t) name)
--      ELambda a ps e -> do
--        ELambda a (apply sub ps) (apply sub e)
--      ERecursiveLet a p e1 e2 -> do
--        ERecursiveLet a (apply sub p) (apply sub e1) (apply sub e2)
--      ELet a gs e1 -> do
--        ELet a (apply sub gs) (apply sub e1)
--      EIf a t e1 e2 e3 -> do
--        EIf a (apply sub t) (apply sub e1) (apply sub e2) (apply sub e3)
--      EApplication a t e1 es -> do
--        EApplication a (apply sub t) (apply sub e1) (apply sub es)
--      EMatch a t e cs ->
--        EMatch a (apply sub t) (apply sub e) (apply sub cs)
--      ECompiledMatch a t e es ->
--        ECompiledMatch a (apply sub t) (apply sub e) (apply sub es)
--      EUnaryOperator a (t, op) ->
--        EUnaryOperator a (apply sub t, op)
--      EBinaryOperator a (t, op) ->
--        EBinaryOperator a (apply sub t, op)
--      ESelect a (Label t name) e ->
--        ESelect a (Label (apply sub t) name) (apply sub e)
--      ERecord a t d e ->
--        ERecord a (apply sub t) (apply sub d) (apply sub e)
--      EListCons a t e1 e2 ->
--        EListCons a (apply sub t) (apply sub e1) (apply sub e2)
--      EListLiteral a t es ->
--        EListLiteral a (apply sub t) (apply sub es)
--      EFold a t es cs e ->
--        EFold a (apply sub t) (apply sub es) (apply sub cs) (apply sub e)
--      e@ELiteral{} ->
--        e
--
--instance Substitutable (Function Expression a IndexedType) where
--  apply sub =
--    \case
--      Function loc (Uses ts t) ps e ->
--        Function loc (Uses (apply sub ts) (apply sub t)) (apply sub ps) (apply sub e)
--
--instance Substitutable (Constant Expression a IndexedType) where
--  apply sub =
--    \case
--      Constant loc (Uses ts t) e ->
--        Constant loc (Uses (apply sub ts) (apply sub t)) (apply sub e)
--
--instance (Substitutable t) => Substitutable (Uses t) where
--  apply sub =
--    \case
--      Uses ts t ->
--        Uses (apply sub ts) (apply sub t)
--
--instance Substitutable (Definition a k IndexedType) where
--  apply sub =
--    \case
--      DFunction a (Function a1 u ps e) ->
--        DFunction a (Function a1 (apply sub u) (apply sub ps) (apply sub e))
--      DConstant a (Constant a1 u e) ->
--        DConstant a (Constant a1 (apply sub u) (apply sub e))
--      DAnnotation a d ->
--        DAnnotation a (apply sub d)
--      _ ->
--        error "TODO"

newtype Substitution = Substitution {substitutionMap :: IndexMap IndexedType}
  deriving (Show, Eq, Ord, Read)

instance Semigroup Substitution where
  s1 <> s2 = Substitution (s3 <> substitutionMap s1)
   where
    s3 = apply s1 (substitutionMap s2)

instance Monoid Substitution where
  mempty = Substitution mempty

{-# INLINE substitutionIndex #-}
substitutionIndex :: TypeIndex Kind -> Substitution -> Maybe IndexedType
substitutionIndex TypeIndex{..} Substitution{..} = Map.lookup typeIndexId substitutionMap

{-# INLINE removeSubstitution #-}
removeSubstitution :: TypeIndex Kind -> Substitution -> Substitution
removeSubstitution TypeIndex{..} Substitution{..} = Substitution (Map.delete typeIndexId substitutionMap)

{-# INLINE mapsTo #-}
mapsTo :: Int -> IndexedType -> Substitution
mapsTo index = Substitution . Map.singleton index

{-# INLINE fromList #-}
fromList :: [(Int, IndexedType)] -> Substitution
fromList = Substitution . Map.fromList

normalizeTypeIndexes :: (Substitutable s, TypeIndexed Kind s) => s -> s
normalizeTypeIndexes a = apply (fromList sub) a
 where
  sub = do
    (n, TypeIndex k t) <- zip [0 ..] (Set.toList (typeIndexesIn a))
    pure (t, TVariable (TypeIndex k n))
