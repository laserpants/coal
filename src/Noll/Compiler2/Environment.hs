{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Compiler2.Environment (buildEnvironments, buildAliasEnv, buildInstanceEnvironment) where

import Control.Monad.State (evalState, execState, modify)
import Data.Map.Strict (Map)
import Lang.Common.Environment (Environment (..))
import Lang.Utils (Dictionary, Set, traverse_, (<$$>))
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Compiler2.Internal (Compiler2Environment (..))
import Noll.Compiler2.Parameterized
import Noll.Language
import Noll.Module.Definition
import Noll.SystemF.Substitution (mapsTo, substituteInScheme)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment

buildEnvironments :: [Definition a k t] -> Compiler2Environment TypeIndex Kind IndexedType
buildEnvironments defs =
  Compiler2Environment
    { compiler2DataConstructorEnv = dataConstructorEnv
    , compiler2TypeConstructorEnv = typeConstructorEnv
    , compiler2TraitEnvironment = traitEnvironment
    , compiler2InstanceEnvironment = instanceEnvironment
    , compiler2AliasEnv = aliasEnv
    }
 where
  instanceEnvironment = buildInstanceEnvironment typeConstructorEnv traitEnvironment defs
  aliasEnv = buildAliasEnv defs
  traitEnvironment = buildTraitEnvironment typeConstructorEnv defs
  dataConstructorEnv = buildDataConstructorEnv typeConstructorEnv defs
  typeConstructorEnv = buildTypeConstructorEnv defs

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
    t1 = evalState (instantiateVars [] env t) (0 :: Int)

buildTypeConstructorEnv :: [Definition a k t] -> Environment Kind
buildTypeConstructorEnv = Environment.fromList . concatMap go
 where
  go =
    \case
      DType name ps _ ->
        [
          ( name
          , foldr KArrow KType (replicate (length ps) KType)
          )
        ]
      _ ->
        []

buildTraitEnvironment :: Environment Kind -> [Definition a k t] -> Environment (TypeIndex Kind, Environment (Scheme TypeIndex Kind IndexedType))
buildTraitEnvironment env = Environment.fromList . concatMap go
 where
  go =
    \case
      DTrait name _ (Parameter k n) ds ->
        [
          ( name
          , (TypeIndex k 0, Environment.fromList (f <$$> ds))
          )
        ]
       where
        f t =
          let t1 = evalState (instantiateVars [(n, TypeIndex k 0)] env t) (1 :: Int)
           in Forall (typeIndexesIn t1) [] t1
      _ ->
        []

buildAliasEnv :: [Definition a k t] -> AliasEnvironment
buildAliasEnv = Environment.fromList . concatMap go
 where
  go =
    \case
      DTypeAlias name ps t ->
        [
          ( name
          ,
            ( parameterName <$> ps
            , t
            )
          )
        ]
      _ ->
        []

buildInstanceEnvironment ::
  Environment Kind ->
  Environment (TypeIndex Kind, Environment (Scheme TypeIndex Kind IndexedType)) ->
  [Definition a k t] ->
  Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
buildInstanceEnvironment env1 env2 ds = execState (traverse_ go ds) mempty
 where
  go =
    \case
      DInstance name t _ ->
        case Environment.lookup name env2 of
          Just (TypeIndex _ ix, env3) -> do
            modify (Environment.insertWith Map.union name val)
           where
            val = Map.singleton t1 (Map.fromList (substituteInScheme (ix `mapsTo` t1) <$$> fs))
            fs = Environment.toList env3
            t1 = evalState (instantiateVars [] env1 t) (freshId fs)
            freshId = freshIdIn . indexSet . fmap snd
          Nothing ->
            error ("Trait '" <> Text.unpack name <> "' not in scope.")
      _ ->
        pure ()

indexSet :: [Scheme TypeIndex Kind t] -> Set (TypeIndex Kind)
indexSet = Set.unions . fmap vars where vars (Forall vs _ _) = vs
