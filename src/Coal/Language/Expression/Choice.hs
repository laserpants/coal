{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Expression.Choice (Choice (..), Guard (..)) where

import Coal.Common.FreeVars (FreeVars (..))
import Data.Binary (Binary)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import qualified Data.Set as Set
import GHC.Generics (Generic)

newtype Guard e a t = CGuard {guardExpression :: e a t}
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)

instance (Ord t, Data a, Data t, Data (e a t), Typeable e) => FreeVars (Guard e a t) t where
  freeIn = Set.fromList . universeBi

instance (Binary (e a t), Binary a, Binary t) => Binary (Guard e a t)

data Choice e a t = CPlain a [Guard e a t] (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)

instance (Ord t, Data a, Data t, Data (e a t), Typeable e) => FreeVars (Choice e a t) t where
  freeIn = Set.fromList . universeBi

instance (Binary (e a t), Binary a, Binary t) => Binary (Choice e a t)
