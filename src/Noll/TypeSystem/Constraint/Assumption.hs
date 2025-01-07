{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Constraint.Assumption (
  Assumption (..),
  assumptionNameIs,
  assumptionNameIsOneOf,
  assumptionNameIsNotOneOf,
) where

import Noll.TypeSystem.Substitution (Substitutable (..))
import Noll.Utils (Name)

data Assumption t = Assumption
  { assumptionName :: Name
  , assumptionType :: t
  }
  deriving (Show, Eq, Ord, Read)

instance (Substitutable t) => Substitutable (Assumption t) where
  apply sub (Assumption name t) = Assumption name (apply sub t)

{-# INLINE assumptionNameIs #-}
assumptionNameIs :: Name -> Assumption t -> Bool
assumptionNameIs name Assumption{..} = assumptionName == name

{-# INLINE assumptionNameIsOneOf #-}
assumptionNameIsOneOf :: [Name] -> Assumption t -> Bool
assumptionNameIsOneOf names Assumption{..} = assumptionName `elem` names

{-# INLINE assumptionNameIsNotOneOf #-}
assumptionNameIsNotOneOf :: [Name] -> Assumption t -> Bool
assumptionNameIsNotOneOf names Assumption{..} = assumptionName `notElem` names
