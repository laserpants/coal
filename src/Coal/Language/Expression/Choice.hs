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

newtype Guard e a s t = CGuard {guardExpression :: e a s t}
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

instance (Ord t, Data a, Data s, Data t, Data (e a s t), Typeable e) => FreeVars (Guard e a s t) t where
  freeIn = Set.fromList . universeBi

instance (Binary (e a s t), Binary a, Binary s, Binary t) => Binary (Guard e a s t)

data Choice e a s t = CPlain a [Guard e a s t] (e a s t)
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

instance (Ord t, Data a, Data s, Data t, Data (e a s t), Typeable e) => FreeVars (Choice e a s t) t where
  freeIn = Set.fromList . universeBi

instance (Binary (e a s t), Binary a, Binary s, Binary t) => Binary (Choice e a s t)
