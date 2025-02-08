{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.HasTag (HasTag (..)) where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (universeBi)
import Noll.Language.Expression (Expression (..))
import Noll.Language.Pattern (Pattern (..))

class HasTag t a where
  tag :: t -> a

instance (Data a, Data t) => HasTag (Expression a t) a where
  tag = head . universeBi

instance (Data a, Data t) => HasTag (Pattern a t) a where
  tag = head . universeBi
