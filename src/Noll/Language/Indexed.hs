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

import Control.Monad.State (State)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set, singleton)
import Noll.Common.Supply (supply)
import Noll.Label (Label (..))
import Noll.Language.Expression (Clause (..), CompiledClause (..), Expression (..))
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

instance (Ord k, Data t, Data k, TypeIndexed k t) => TypeIndexed k (Label t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data t, Data k, TypeIndexed k t) => TypeIndexed k (Row TypeIndex k t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data t, Data k, TypeIndexed k t) => TypeIndexed k (Intrinsic t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data k) => TypeIndexed k (Type TypeIndex k) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data k, Data t, Data a, TypeIndexed k t) => TypeIndexed k (Pattern a t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Binding Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Guard Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Choice Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Clause Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (CompiledClause Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data k, Data a) => TypeIndexed k (Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Function Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Constant Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data t, Data k, TypeIndexed k t) => TypeIndexed k (Uses t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Definition a k (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Scheme TypeIndex k t) where
  typeIndexesIn =
    \case
      Forall qs ps t ->
        notBoundIn qs (typeIndexesIn t <> typeIndexesIn ps)

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
indexed = traverse (fmap tVar . const supply)
 where
  tVar = TVariable . TypeIndex KType
