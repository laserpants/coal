{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Language.Expression.Binding

Let-binding constructs for pattern and function bindings.
-}
module Coal.Language.Expression.Binding (Binding (..)) where

import Coal.Common.FreeVars (BoundVars (..), FreeVars (..), exceptNames)
import Coal.Language.Pattern (Pattern (..))
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Extras (Name)
import GHC.Generics (Generic)

data Binding e a s t
  = BPattern a (Pattern a s t) (e a s t)
  | BFunction a Name (NonEmpty (Pattern a s t)) (e a s t)
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    , Generic
    )

instance (Binary a, Binary s, Binary t, Binary (e a s t)) => Binary (Binding e a s t)

instance (Data a, Data s, Data t) => BoundVars (Binding e a s t) where
  boundIn =
    \case
      BPattern _ p _ ->
        Set.fromList (universeBi p)
      BFunction _ name _ _ ->
        Set.singleton name

instance (Data a, Data s, Data t, FreeVars (e a s t) t) => FreeVars (Binding e a s t) t where
  freeIn =
    \case
      BPattern _ _ e ->
        freeIn e
      BFunction _ _ ps e ->
        freeIn e `exceptNames` boundIn ps
