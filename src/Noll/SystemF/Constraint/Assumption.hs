{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Constraint.Assumption (
  Assumption (..),
  assumptionNameIs,
  assumptionNameIsOneOf,
  assumptionNameIsNotOneOf,
) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transformBi)
import Noll.SystemF.Substitution (Substitutable (..), applyT)
import Noll.Utils (Name)

data Assumption t = Assumption
  { assumptionName :: Name
  , assumptionType :: t
  }
  deriving (Show, Eq, Ord, Read, Data, Typeable)

instance (Data t) => Substitutable (Assumption t) where
  apply = transformBi . applyT

{-# INLINE assumptionNameIs #-}
assumptionNameIs :: Name -> Assumption t -> Bool
assumptionNameIs name Assumption{..} = assumptionName == name

{-# INLINE assumptionNameIsOneOf #-}
assumptionNameIsOneOf :: [Name] -> Assumption t -> Bool
assumptionNameIsOneOf names Assumption{..} = assumptionName `elem` names

{-# INLINE assumptionNameIsNotOneOf #-}
assumptionNameIsNotOneOf :: [Name] -> Assumption t -> Bool
assumptionNameIsNotOneOf names Assumption{..} = assumptionName `notElem` names
