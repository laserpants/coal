{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Compiler2.Environment (
  DataConstructorEnv,
  TypeConstructorEnv,
  TraitEnvironment,
  InstanceEnvironment,
  Compiler2Environment (..),
  emptyCompiler2Environment,
  buildEnvironment,
  buildAliasEnv,
  buildInstanceEnvironment,
) where

import Control.Monad.State (evalState, execState, modify)
import Data.Map.Strict (Map)
import Lang.Common.Environment (Environment (..))
import Lang.Utils (Dictionary, Set, traverse_, (<$$>))
import Noll.Compiler2.Parameterized
import Noll.Compiler2.Transform.Type.AliasExpansion
import Noll.Language
import Noll.Module.Definition
import Noll.SystemF.Substitution (mapsTo, substituteInScheme)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment

type DataConstructorEnv = Environment (Constructor TypeIndex Kind IndexedType)
type TypeConstructorEnv = Environment Kind
type TraitEnvironment = Environment (TypeIndex Kind, Environment (Scheme TypeIndex Kind IndexedType))
type InstanceEnvironment = Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))

data Compiler2Environment = Compiler2Environment
  { compiler2DataConstructorEnv :: DataConstructorEnv
  , compiler2TypeConstructorEnv :: TypeConstructorEnv
  , compiler2TraitEnvironment :: TraitEnvironment
  , compiler2InstanceEnvironment :: InstanceEnvironment
  , compiler2AliasEnv :: AliasEnvironment
  }
  deriving (Show, Eq, Ord, Read)

emptyCompiler2Environment :: Compiler2Environment
emptyCompiler2Environment =
  Compiler2Environment
    { compiler2DataConstructorEnv = mempty
    , compiler2TypeConstructorEnv = mempty
    , compiler2TraitEnvironment = mempty
    , compiler2InstanceEnvironment = mempty
    , compiler2AliasEnv = mempty
    }

buildEnvironment :: [Definition a k t] -> Compiler2Environment
buildEnvironment defs =
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

buildDataConstructorEnv :: TypeConstructorEnv -> [Definition a k t] -> DataConstructorEnv
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

buildTypeConstructorEnv :: [Definition a k t] -> TypeConstructorEnv
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

buildTraitEnvironment :: TypeConstructorEnv -> [Definition a k t] -> TraitEnvironment
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

buildInstanceEnvironment :: TypeConstructorEnv -> TraitEnvironment -> [Definition a k t] -> InstanceEnvironment
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
