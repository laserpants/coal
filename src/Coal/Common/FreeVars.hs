{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Common.FreeVars (
  BoundVars (..),
  FreeVars (..),
  exceptNames,
  freeSet,
  notConstructor,
  notOneOf,
) where

import Coal.Common.Label (Label (..), labelName)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set, singleton)
import qualified Data.Set as Set
import Extras (Dictionary, Map, Name, isConstructor)

class BoundVars b where
  boundIn :: b -> Set Name

instance (BoundVars b) => BoundVars (Dictionary b) where
  boundIn = Set.unions . fmap boundIn

instance (BoundVars b) => BoundVars (Maybe b) where
  boundIn = Set.unions . fmap boundIn

instance (BoundVars b) => BoundVars [b] where
  boundIn = Set.unions . fmap boundIn

instance (BoundVars b) => BoundVars (NonEmpty b) where
  boundIn = Set.unions . fmap boundIn

instance BoundVars (Label t) where
  boundIn (Label _ name) = singleton name

class FreeVars f t where
  freeIn :: f -> Set (Label t)

instance (FreeVars (Label t) t) where
  freeIn = Set.singleton

instance (Ord t, FreeVars f t) => FreeVars (Maybe f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, FreeVars f t) => FreeVars [f] t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, FreeVars f t) => FreeVars (NonEmpty f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, FreeVars f t) => FreeVars (Map a f) t where
  freeIn = Set.unions . fmap freeIn

instance (Ord t, FreeVars s t) => FreeVars (Set s) t where
  freeIn = Set.unions . Set.map freeIn

exceptNames :: (Foldable f) => Set (Label a) -> f Name -> Set (Label a)
exceptNames free bound = Set.filter (`notOneOf` bound) free

{-# INLINE notOneOf #-}
notOneOf :: (Foldable f) => Label a -> f Name -> Bool
notOneOf = notElem . labelName

notConstructor :: Label t -> Bool
notConstructor = not . isConstructor . labelName

freeSet :: (Foldable f, FreeVars e t) => f Name -> e -> Set (Label t)
freeSet names obj = Set.filter notConstructor (freeIn obj `exceptNames` names)
