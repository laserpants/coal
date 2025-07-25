{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Expression.Binding (Binding (..)) where

import Coal.Common.FreeVars (BoundVars (..), FreeVars (..), exceptNames)
import Coal.Common.List1 (List1)
import Coal.Language.Pattern (Pattern (..))
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Extra (Name)

import qualified Data.Set as Set

data Binding e a t
  = BPattern a (Pattern a t) (e a t)
  | BFunction a Name (List1 (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

instance (Data a, Data t) => BoundVars (Binding e a t) where
  boundIn =
    \case
      BPattern _ p _ ->
        Set.fromList (universeBi p)
      BFunction{} ->
        error "TODO"

instance (Data a, Data t, FreeVars (e a t) t) => FreeVars (Binding e a t) t where
  freeIn =
    \case
      BPattern _ _ e ->
        freeIn e
      BFunction _ _ ps e ->
        freeIn e `exceptNames` boundIn ps
