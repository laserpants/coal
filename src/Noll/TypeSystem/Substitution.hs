{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Substitution where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Noll.Language.Trait (Trait (..))
import Noll.Language.Type.Index (TypeIndex)
import Noll.Utils (IndexMap)

class Substitutable s t where
  apply :: Substitution t -> s -> s

instance (Substitutable s t) => Substitutable (Map a s) t where
  apply = fmap . apply

instance (Substitutable s t) => Substitutable [s] t where
  apply = fmap . apply

instance (Substitutable s t) => Substitutable (NonEmpty s) t where
  apply = fmap . apply

instance (Substitutable s t) => Substitutable (Maybe s) t where
  apply = fmap . apply

instance (Substitutable s t) => Substitutable (Trait s) t where
  apply = fmap . apply

instance (Substitutable s t) => Substitutable (TypeIndex s) t where
  apply = fmap . apply

instance (Ord s, Substitutable s t) => Substitutable (Set s) t where
  apply = Set.map . apply

newtype Substitution s = Substitution {substitutionMap :: IndexMap s}
  deriving (Show, Eq, Ord, Read)
