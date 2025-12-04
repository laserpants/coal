{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Coal.Language.Type.Indexed (
  TypeIndexed (..),
  typeIdsIn,
  notBoundIn,
  freshIdIn,
  indexed,
  scheme,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supply)
import Coal.Language.Expression (Clause (..), CompiledClause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Module (ConstantDef, Definition, FunctionDef, Module)
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Trait (..), With (..))
import Coal.Language.Type (IndexedType, Type (..), TypeIndex (..))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Control.Monad.State (State)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set, singleton)
import Extras.Data.Set (unionMap)

import qualified Data.Set as Set

class TypeIndexed k t where
  typeIndexesIn :: t -> Set (TypeIndex k)

instance TypeIndexed k (TypeIndex k) where
  typeIndexesIn = singleton

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Map a t) where
  typeIndexesIn = unionMap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Maybe t) where
  typeIndexesIn = unionMap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k [t] where
  typeIndexesIn = unionMap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (NonEmpty t) where
  typeIndexesIn = unionMap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Trait t) where
  typeIndexesIn = unionMap typeIndexesIn

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Set t) where
  typeIndexesIn = Set.unions . Set.map typeIndexesIn

instance (Ord k, Data t, Data k) => TypeIndexed k (Label t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data t, Data k) => TypeIndexed k (Row TypeIndex k t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data k) => TypeIndexed k Intrinsic where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data k) => TypeIndexed k (Type TypeIndex k) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data k, Data t, Data a) => TypeIndexed k (Pattern a t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Binding Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Guard Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Choice Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Clause a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (CompiledClause a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data k, Data a) => TypeIndexed k (Expression a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (FunctionDef a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (ConstantDef a (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data t, Data k) => TypeIndexed k (With t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (Definition a k (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data t, Data k) => TypeIndexed k (Module a k t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, TypeIndexed k t) => TypeIndexed k (Scheme TypeIndex k t) where
  typeIndexesIn =
    \case
      Forall qs ps t ->
        notBoundIn qs (typeIndexesIn t <> typeIndexesIn ps)

notBoundIn :: Set (TypeIndex k) -> Set (TypeIndex k) -> Set (TypeIndex k)
notBoundIn set = Set.filter notBound
 where
  notBound index = typeIndexId index `notElem` Set.map typeIndexId set

typeIdsIn :: (TypeIndexed Kind t) => t -> Set Int
typeIdsIn t = Set.map typeIndexId (typeIndexesIn @Kind t)

freshIdIn :: (TypeIndexed Kind t) => t -> Int
freshIdIn t
  | null typeIdSet = 0
  | otherwise = succ (maximum typeIdSet)
 where
  typeIdSet = typeIdsIn t

indexed :: (Traversable t) => t a -> State Int (t IndexedType)
indexed = traverse (fmap tVar . const supply)
 where
  tVar = TVariable . TypeIndex KType

{-# INLINE scheme #-}
scheme :: (Ord k, TypeIndexed k t) => [Trait t] -> t -> Scheme TypeIndex k t
scheme ts t = Forall (typeIndexesIn t <> typeIndexesIn ts) ts t
