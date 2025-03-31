{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression.Binding (Binding (..)) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Lang.FreeVars (BoundVars (..), FreeVars (..), exceptNames)
import Noll.Common.List1 (List1)
import Noll.Language.Pattern (Pattern (..))
import Noll.Utils (Name)

import qualified Data.Set as Set

data Binding e a t
  = BPattern a (Pattern a t) (e a t)
  | BFunction a Name (List1 (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

instance (Data a, Data t, Typeable e, Data (e a t)) => BoundVars (Binding e a t) where
  boundIn = Set.fromList . universeBi

instance (Data a, Data t, FreeVars (e a t) t) => FreeVars (Binding e a t) t where
  freeIn =
    \case
      BPattern _ _ e ->
        freeIn e
      BFunction _ _ ps e ->
        freeIn e `exceptNames` boundIn ps
