{-# LANGUAGE LambdaCase #-}

module Noll.Compiler2.Environment where

import Lang.Common.Environment (Environment (..))
import Lang.Utils (Name)
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Language
import Noll.Module.Definition

import qualified Lang.Common.Environment as Environment

buildDataConstructorEnv :: Environment Kind -> [Definition a k t] -> Environment (Constructor o Kind IndexedType)
buildDataConstructorEnv env =
  undefined
 where
  go =
    \case
      DType name ps cs ->
        let zz = xxx <$> cs
         in undefined
      _ ->
        []

xxx :: Constructor Parameter () (Type Parameter ()) -> (Name, Constructor o Kind IndexedType)
xxx (Constructor n a s) = (n, Constructor n a (zabc s))

zabc :: Scheme Parameter () (Type Parameter ()) -> Scheme o Kind IndexedType
zabc = undefined

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

-- TODO
buildAliasEnv :: a -> AliasEnvironment
buildAliasEnv =
  undefined
