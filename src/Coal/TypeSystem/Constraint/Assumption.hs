{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.TypeSystem.Constraint.Assumption
Description: Type assumptions for constraint generation

This module defines type assumptions that associate names with types during
the constraint generation phase of type inference. Assumptions represent the
expected types of variables and are used to generate equality and unification
constraints.
-}
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
assumptionNameIs name Assumption{assumptionName} = assumptionName == name

{-# INLINE assumptionNameIsOneOf #-}
assumptionNameIsOneOf :: [Name] -> Assumption a t -> Bool
assumptionNameIsOneOf names Assumption{assumptionName} = assumptionName `elem` names

{-# INLINE assumptionNameIsNotOneOf #-}
assumptionNameIsNotOneOf :: [Name] -> Assumption a t -> Bool
assumptionNameIsNotOneOf names Assumption{assumptionName} = assumptionName `notElem` names
