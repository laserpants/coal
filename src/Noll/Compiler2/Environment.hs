{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Compiler2.Environment where

import Control.Monad.State (evalState)
import Lang.Common.Environment (Environment (..))
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Compiler2.Params
import Noll.Language
import Noll.Module.Definition

import qualified Lang.Common.Environment as Environment

buildConstructorEnvs :: [Definition a k t] -> (Environment Kind, Environment (Constructor TypeIndex Kind IndexedType))
buildConstructorEnvs defs = (e1, e2)
 where
  e2 = buildDataConstructorEnv e1 defs
  e1 = buildTypeConstructorEnv defs

buildDataConstructorEnv :: Environment Kind -> [Definition a k t] -> Environment (Constructor TypeIndex Kind IndexedType)
buildDataConstructorEnv env = Environment.fromList . concatMap go
 where
  go =
    \case
      DType _ _ cs ->
        translateConstructor <$> cs
      _ ->
        []
  translateConstructor (Constructor n a s) =
    (n, Constructor n a (translateScheme s))
  translateScheme (Forall _ _ t) =
    Forall (typeIndexesIn t1) [] t1
   where
    t1 = evalState (instantiateVars env t) (0 :: Int)

buildTypeConstructorEnv :: [Definition a k t] -> Environment Kind
buildTypeConstructorEnv = Environment.fromList . concatMap go
 where
  go =
    \case
      DType name ps _ ->
        [(name, foldr KArrow KType (replicate (length ps) KType))]
      _ ->
        []

buildTraitEnvironment :: Environment Kind -> [Definition a k t] -> Environment (TypeIndex Kind, Environment (Scheme TypeIndex Kind IndexedType))
buildTraitEnvironment env = Environment.fromList . concatMap go
 where
  go =
    \case
      DTrait name _ t ds ->
          undefined
         where
          t1 = evalState (instantiateVars env t) (0 :: Int)
      _ ->
        []

-- TODO
buildAliasEnv :: a -> AliasEnvironment
buildAliasEnv =
  undefined
