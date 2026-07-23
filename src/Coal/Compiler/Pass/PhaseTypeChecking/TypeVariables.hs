{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Compiler.Pass.PhaseTypeChecking.TypeVariables
Description: Utilities for collecting type variables from types

This module provides helper functions to collect type variable names from
type expressions. These are used for validating that all type variables
used in type definitions and type aliases are properly bound in the
parameter list.
-}
module Coal.Compiler.Pass.PhaseTypeChecking.TypeVariables (
  collectTypeVarNames,
  collectTypeVarNamesInRow,
  collectTypeConstructorNames,
) where

import Coal.Language.Type (Parameter (..), Type (..))
import Coal.Language.Type.Row (Row (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name)

{- | Collect all type variable names from a type
Used to check for unbound type variables in type definitions and aliases
-}
collectTypeVarNames :: Type Parameter k -> Set Name
collectTypeVarNames =
  \case
    TVariable Parameter{..} -> Set.singleton parameterName
    TArrow t1 t2 -> collectTypeVarNames t1 <> collectTypeVarNames t2
    TApplication _ t1 t2 -> collectTypeVarNames t1 <> collectTypeVarNames t2
    TRecord t -> collectTypeVarNames t
    TRow row -> collectTypeVarNamesInRow row
    TAlias _ ts t -> foldMap collectTypeVarNames ts <> collectTypeVarNames t
    TConstructor{} -> Set.empty
    TIntrinsic{} -> Set.empty

-- | Collect all type variable names from a row type
collectTypeVarNamesInRow :: Row Parameter k (Type Parameter k) -> Set Name
collectTypeVarNamesInRow =
  \case
    RExtend _ t row -> collectTypeVarNames t <> collectTypeVarNamesInRow row
    RVariable Parameter{..} -> Set.singleton parameterName
    RNil -> Set.empty

{- | Collect all type constructor names from a type
Used to check for undefined type constructors in trait interface schemes
-}
collectTypeConstructorNames :: Type Parameter k -> Set Name
collectTypeConstructorNames =
  \case
    TConstructor _ name -> Set.singleton name
    TArrow t1 t2 -> collectTypeConstructorNames t1 <> collectTypeConstructorNames t2
    TApplication _ t1 t2 -> collectTypeConstructorNames t1 <> collectTypeConstructorNames t2
    TRecord t -> collectTypeConstructorNames t
    TRow row -> collectTypeConstructorNamesInRow row
    TAlias _ ts t -> foldMap collectTypeConstructorNames ts <> collectTypeConstructorNames t
    TVariable{} -> Set.empty
    TIntrinsic{} -> Set.empty

-- | Collect all type constructor names from a row type
collectTypeConstructorNamesInRow :: Row Parameter k (Type Parameter k) -> Set Name
collectTypeConstructorNamesInRow =
  \case
    RExtend _ t row -> collectTypeConstructorNames t <> collectTypeConstructorNamesInRow row
    RVariable{} -> Set.empty
    RNil -> Set.empty
