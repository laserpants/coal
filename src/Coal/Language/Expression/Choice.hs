{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Expression.Choice (Choice (..), Guard (..)) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Coal.Common.FreeVars (FreeVars (..))
import Coal.Common.List1 (List1)
import Coal.Language.Pattern (Pattern)

import qualified Data.Set as Set

newtype Guard e a t = CGuard (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

instance (Ord t, Data a, Data t, Data (e a t), Typeable e) => FreeVars (Guard e a t) t where
  freeIn = Set.fromList . universeBi

data Choice e a t
  = CPlain a [Guard e a t] (e a t)
  | CLambda a (List1 (Pattern a t)) [Guard e a t] (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

instance (Ord t, Data a, Data t, Data (e a t), Typeable e) => FreeVars (Choice e a t) t where
  freeIn = Set.fromList . universeBi
