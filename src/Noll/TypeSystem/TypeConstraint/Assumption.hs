{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeConstraint.Assumption (Assumption (..), assumptionNameIs) where

import Noll.Utils (Name)

data Assumption t = Assumption
  { assumptionName :: Name
  , assumptionType :: t
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE assumptionNameIs #-}
assumptionNameIs :: Name -> Assumption t -> Bool
assumptionNameIs name Assumption{..} = assumptionName == name
