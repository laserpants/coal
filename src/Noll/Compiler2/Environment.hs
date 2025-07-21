{-# LANGUAGE LambdaCase #-}

module Noll.Compiler2.Environment where

import Noll.Language
import Noll.Module.Definition
import Lang.Common.Environment (Environment (..))
import Noll.Compiler.Transform.Type.AliasExpansion

import qualified Lang.Common.Environment as Environment

buildDataConstructorEnv :: Environment Kind -> [Definition a k t] -> Environment (Constructor o k t)
buildDataConstructorEnv =
  undefined
   where
    go =
      \case
        DType name ps cs ->
          undefined
        _ ->
          []

buildTypeConstructorEnv :: [Definition a k t] -> Environment Kind
buildTypeConstructorEnv = Environment.fromList . concatMap go
   where
    go =
      \case
        DType name ps _ ->
          [(name, foldr KArrow KType (replicate (length ps) KType))]
        _ ->
          []

buildTraitEnvironment :: a -> Environment (TypeIndex Kind, Environment (Scheme TypeIndex Kind IndexedType))
buildTraitEnvironment =
  undefined
   where
    go =
      \case
        DTrait name ts t ds ->
          undefined
        _ ->
          []

buildAliasEnv :: a -> AliasEnvironment
buildAliasEnv =
  undefined
