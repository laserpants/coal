{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Noll.Language.Indexed (
  TypeIndexed (..),
  typeIdsIn,
  notBoundIn,
  freshIdIn,
  indexed,
) where

import Control.Monad.State (State, evalState)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set, singleton)
import Noll.Common.Supply (supply)
import Noll.Label (Label (..))
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..), Guard (..))
import Noll.Language.Module.Constant (Constant (..))
import Noll.Language.Module.Definition (Definition (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Trait (Trait (..), Uses (..))
import Noll.Language.Type (Type (..), TypeIndex (..))
import Noll.Language.Type.Intrinsic (Intrinsic (..))
import Noll.Language.Type.Kind (Kind (..))
import Noll.Language.Type.Row (Row (..))
import Noll.Language.Type.Scheme (Scheme (..))
import Noll.Utils (unionMap)

import qualified Data.Set as Set

class TypeIndexed k t | t -> k where
  typeIndexesIn :: t -> Set (TypeIndex k)

instance TypeIndexed k (TypeIndex k) where
  typeIndexesIn = singleton

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Map a t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Maybe t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k [t] where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (NonEmpty t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Trait t) where
  typeIndexesIn = Set.unions . fmap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Set t) where
  typeIndexesIn = Set.unions . Set.map typeIndexesIn

instance (TypeIndexed k t) => TypeIndexed k (Label t) where
  typeIndexesIn =
    \case
      Label t _ ->
        typeIndexesIn t

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Row TypeIndex k t) where
  typeIndexesIn =
    \case
      RExtend _ t row ->
        typeIndexesIn t <> typeIndexesIn row
      RVariable t ->
        typeIndexesIn t
      RNil ->
        mempty

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Intrinsic t) where
  typeIndexesIn =
    Set.unions . fmap typeIndexesIn

instance (Ord k) => TypeIndexed k (Type TypeIndex k) where
  typeIndexesIn =
    \case
      TApplication _ t ts ->
        typeIndexesIn t <> typeIndexesIn ts
      TArrow t1 t2 ->
        typeIndexesIn t1 <> typeIndexesIn t2
      TConstructor{} ->
        mempty
      TIntrinsic t -> do
        typeIndexesIn t
      TRow row ->
        typeIndexesIn row
      TVariable t ->
        typeIndexesIn t
      TAlias _ _ t ->
        typeIndexesIn t

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Pattern a t) where
  typeIndexesIn =
    \case
      PAnnotation _ _ p ->
        typeIndexesIn p
      PVariable _ (Label t _) ->
        typeIndexesIn t
      PConstructor _ (Label t _) ps ->
        typeIndexesIn t <> typeIndexesIn ps
      POr _ t p1 p2 ->
        typeIndexesIn t <> typeIndexesIn p1 <> typeIndexesIn p2
      PRecord _ t d p ->
        typeIndexesIn t <> typeIndexesIn d <> typeIndexesIn p
      PShorthand _ (Label t _) ->
        typeIndexesIn t
      PListCons _ t p1 p2 ->
        typeIndexesIn t <> typeIndexesIn p1 <> typeIndexesIn p2
      PListLiteral _ t ps ->
        typeIndexesIn t <> typeIndexesIn ps
      PAny _ t ->
        typeIndexesIn t
      PAtVariable _ (Label t _) ->
        typeIndexesIn t

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Scheme TypeIndex k t) where
  typeIndexesIn =
    \case
      Forall qs ps t ->
        notBoundIn qs (typeIndexesIn t <> typeIndexesIn ps)

instance (Ord k) => TypeIndexed k (Binding Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      BPattern _ p e ->
        typeIndexesIn p <> typeIndexesIn e
      BFunction _ _ ps e ->
        typeIndexesIn ps <> typeIndexesIn e

instance (Ord k) => TypeIndexed k (Guard Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      CGuard e ->
        typeIndexesIn e

instance (Ord k) => TypeIndexed k (Choice Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      CPlain _ gs e ->
        typeIndexesIn gs <> typeIndexesIn e

instance (Ord k) => TypeIndexed k (Clause Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      EClause _ p cs ->
        typeIndexesIn p <> typeIndexesIn cs

instance (Ord k) => TypeIndexed k (Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      EAnnotation _ _ e ->
        typeIndexesIn e
      EConstructor _ (Label t _) ->
        typeIndexesIn t
      EVariable _ (Label t _) ->
        typeIndexesIn t
      ELambda _ ps e ->
        typeIndexesIn ps <> typeIndexesIn e
      ERecursiveLet _ p e1 e2 ->
        typeIndexesIn p <> typeIndexesIn e1 <> typeIndexesIn e2
      ELet _ gs e1 ->
        typeIndexesIn gs <> typeIndexesIn e1
      EIf _ t e1 e2 e3 ->
        typeIndexesIn t <> typeIndexesIn e1 <> typeIndexesIn e2 <> typeIndexesIn e3
      EApplication _ t e1 es ->
        typeIndexesIn t <> typeIndexesIn e1 <> typeIndexesIn es
      ELiteral{} ->
        mempty
      EListCons _ t e1 e2 ->
        typeIndexesIn t <> typeIndexesIn e1 <> typeIndexesIn e2
      EListLiteral _ t es ->
        typeIndexesIn t <> typeIndexesIn es
      EMatch _ t e cs ->
        typeIndexesIn t <> typeIndexesIn e <> typeIndexesIn cs
      EUnaryOperator _ (t, _) ->
        typeIndexesIn t
      EBinaryOperator _ (t, _) ->
        typeIndexesIn t
      ERecord _ t d e ->
        typeIndexesIn t <> typeIndexesIn d <> typeIndexesIn e
      ESelect _ (Label t _) e ->
        typeIndexesIn t <> typeIndexesIn e
      EFold _ t es cs e ->
        typeIndexesIn t <> typeIndexesIn es <> typeIndexesIn cs <> typeIndexesIn e

instance (Ord k) => TypeIndexed k (Function Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      Function _ (Uses ts t) ps e ->
        typeIndexesIn ts <> typeIndexesIn t <> typeIndexesIn ps <> typeIndexesIn e

instance (Ord k) => TypeIndexed k (Constant Expression a (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      Constant _ (Uses ts t) e ->
        typeIndexesIn ts <> typeIndexesIn t <> typeIndexesIn e

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Uses t) where
  typeIndexesIn =
    \case
      Uses ts t ->
        typeIndexesIn ts <> typeIndexesIn t

instance (Ord k) => TypeIndexed k (Definition a k (Type TypeIndex k)) where
  typeIndexesIn =
    \case
      DFunction _ (Function _ u ps e) ->
        typeIndexesIn u <> typeIndexesIn ps <> typeIndexesIn e
      DConstant _ (Constant _ u e) ->
        typeIndexesIn u <> typeIndexesIn e
      d ->
        error "TODO"

notBoundIn :: Set (TypeIndex k) -> Set (TypeIndex k) -> Set (TypeIndex k)
notBoundIn s = Set.filter notBound
 where
  notBound index = typeIndexId index `notElem` Set.map typeIndexId s

typeIdsIn :: (TypeIndexed k t) => t -> Set Int
typeIdsIn = Set.map typeIndexId . typeIndexesIn

freshIdIn :: (Ord k, TypeIndexed k t) => t -> Int
freshIdIn t
  | null typeIndexSet = 0
  | otherwise = succ (maximum (typeIdsIn typeIndexSet))
 where
  typeIndexSet = typeIndexesIn t

indexed :: (Traversable t) => t a -> State Int (t (Type TypeIndex Kind))
indexed t = traverse (fmap tVar . const supply) t
 where
  tVar = TVariable . TypeIndex KType
