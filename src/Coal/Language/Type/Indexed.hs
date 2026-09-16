{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module: Coal.Language.Type.Indexed

Type indexing and operations for indexed type representations.
-}
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
import Coal.Language.Definition (Definition)
import Coal.Language.Expression (Clause (..), CompiledClause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Qualified (..), Trait (..))
import Coal.Language.Type (IndexedType, Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
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

{- | Explicit hand-written traversals for indexed types, replacing the
@Set.fromList . universeBi@ generic traversals that previously ran here.

'universeBi' walks the full generic structure of every nested 'Data' value
(kinds, names, row tails, ...) to collect 'TypeIndex' occurrences. The
explicit recursion below visits only type structure.
-}
typeIndexesInType :: Type TypeIndex Kind -> Set (TypeIndex Kind)
typeIndexesInType = go
 where
  go = \case
    TApplication _ t1 t2 ->
      go t1 <> go t2
    TArrow t1 t2 ->
      go t1 <> go t2
    TConstructor{} ->
      mempty
    TIntrinsic{} ->
      mempty
    TRecord t ->
      go t
    TRow row ->
      typeIndexesInRow row
    TVariable v ->
      singleton v
    TAlias _ ts t ->
      foldr (\x s -> go x <> s) (go t) ts

typeIndexesInRow :: Row TypeIndex Kind IndexedType -> Set (TypeIndex Kind)
typeIndexesInRow = go
 where
  go = \case
    RExtend _ t r ->
      typeIndexesInType t <> go r
    RVariable v ->
      singleton v
    RNil ->
      mempty

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

instance TypeIndexed Kind (Row TypeIndex Kind IndexedType) where
  typeIndexesIn = typeIndexesInRow

instance TypeIndexed Kind IndexedType where
  typeIndexesIn = typeIndexesInType

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

instance (Ord k, Data a, Data k) => TypeIndexed k (Definition a k (Type TypeIndex k)) where
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
