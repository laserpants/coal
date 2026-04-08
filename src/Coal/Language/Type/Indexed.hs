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
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Qualified (..), Trait (..))
import Coal.Language.Type (IndexedType, Type (..), TypeIndex (..))
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.ProtoLanguage.ProtoDefinition
import Control.Monad.State (State)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set, singleton)
import qualified Data.Set as Set
import Extras.Data.Set (unionMap)

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

instance (Ord k, Data k, Data t, Data a, Data s) => TypeIndexed k (Pattern a s t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k, Data s) => TypeIndexed k (Binding Expression a s (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k, Data s) => TypeIndexed k (Guard Expression a s (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k, Data s) => TypeIndexed k (Choice Expression a s (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k, Data s) => TypeIndexed k (Clause a s (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k, Data s) => TypeIndexed k (CompiledClause a s (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data k, Data a, Data s) => TypeIndexed k (Expression a s (Type TypeIndex k)) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data t, Data k) => TypeIndexed k (Qualified t) where
  typeIndexesIn = Set.fromList . universeBi

instance (Ord k, Data a, Data k) => TypeIndexed k (ProtoDefinition a k (Type TypeIndex k)) where
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
scheme :: (Ord k, TypeIndexed k t) => Set (Trait t) -> t -> Scheme TypeIndex k t
scheme ts t = Forall (typeIndexesIn t <> typeIndexesIn ts) ts t
