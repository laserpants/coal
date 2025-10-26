{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Assumption (
  Assumption (..),
  assumptionNameIs,
  assumptionNameIsOneOf,
  assumptionNameIsNotOneOf,
) where

import Coal.TypeSystem.Substitution (Substitutable (..), applyT)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transformBi)
import Extras (Name)

data Assumption a t = Assumption
  { assumptionMetadata :: a
  , assumptionName :: Name
  , assumptionType :: t
  }
  deriving (Show, Eq, Ord, Read, Data, Typeable)

instance (Data a, Data t) => Substitutable (Assumption a t) where
  apply = transformBi . applyT

{-# INLINE assumptionNameIs #-}
assumptionNameIs :: Name -> Assumption a t -> Bool
assumptionNameIs name Assumption{..} = assumptionName == name

{-# INLINE assumptionNameIsOneOf #-}
assumptionNameIsOneOf :: [Name] -> Assumption a t -> Bool
assumptionNameIsOneOf names Assumption{..} = assumptionName `elem` names

{-# INLINE assumptionNameIsNotOneOf #-}
assumptionNameIsNotOneOf :: [Name] -> Assumption a t -> Bool
assumptionNameIsNotOneOf names Assumption{..} = assumptionName `notElem` names
