{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Compiler.Environment (
  DataConstructorEnvironment,
  TypeConstructorEnvironment,
  TraitEnvironment,
  InstanceEnvironment,
  CompilerEnvironment (..),
  emptyCompilerEnvironment,
  buildEnvironment,
  buildAliasEnvironment,
  buildInstanceEnvironment,
) where

import Control.Monad.State (evalState, execState, modify)
import Data.Map.Strict (Map)
import Noll.Common.Environment (Environment (..))
import Extra (Dictionary, Name, Set, traverse_, (<$$>))
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Compiler.Transform.Type.Parameterized
import Noll.Language
import Noll.Language.Module.Definition
import Noll.TypeSystem.Substitution (mapsTo, substituteInScheme)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Noll.Common.Environment as Environment

type DataConstructorEnvironment = Environment (Constructor TypeIndex Kind IndexedType)
type TypeConstructorEnvironment = Environment Kind
type TraitEnvironment = Environment (TypeIndex Kind, Environment IndexedScheme)
type InstanceEnvironment = Environment (Map IndexedType (Dictionary IndexedScheme))

data CompilerEnvironment = CompilerEnvironment
  { compilerDataConstructorEnvironment :: DataConstructorEnvironment
  , compilerTypeConstructorEnvironment :: TypeConstructorEnvironment
  , compilerTraitEnvironment :: TraitEnvironment
  , compilerInstanceEnvironment :: InstanceEnvironment
  , compilerAliasEnvironment :: AliasEnvironment
  }
  deriving (Show, Eq, Ord, Read)

emptyCompilerEnvironment :: CompilerEnvironment
emptyCompilerEnvironment =
  CompilerEnvironment
    { compilerDataConstructorEnvironment = mempty
    , compilerTypeConstructorEnvironment = mempty
    , compilerTraitEnvironment = mempty
    , compilerInstanceEnvironment = mempty
    , compilerAliasEnvironment = mempty
    }

buildEnvironment :: [Definition a k t] -> CompilerEnvironment
buildEnvironment defs =
  CompilerEnvironment
    { compilerDataConstructorEnvironment = dataConstructorEnvironment
    , compilerTypeConstructorEnvironment = typeConstructorEnvironment
    , compilerTraitEnvironment = traitEnvironment
    , compilerInstanceEnvironment = instanceEnvironment
    , compilerAliasEnvironment = aliasEnvironment
    }
 where
  instanceEnvironment = buildInstanceEnvironment typeConstructorEnvironment traitEnvironment defs
  aliasEnvironment = buildAliasEnvironment defs
  traitEnvironment = buildTraitEnvironment typeConstructorEnvironment defs
  dataConstructorEnvironment = buildDataConstructorEnvironment typeConstructorEnvironment defs
  typeConstructorEnvironment = buildTypeConstructorEnvironment defs

makeEnv :: (Definition a k t -> [(Name, e)]) -> [Definition a k t] -> Environment e
makeEnv f = Environment.fromList . concatMap f

buildDataConstructorEnvironment :: TypeConstructorEnvironment -> [Definition a k t] -> DataConstructorEnvironment
buildDataConstructorEnvironment env =
  makeEnv
    ( \case
        DType _ _ cs ->
          translateConstructor <$> cs
        _ ->
          []
    )
 where
  translateConstructor (Constructor n a s) =
    (n, Constructor n a (translateScheme s))
  translateScheme (Forall _ _ t) =
    Forall (typeIndexesIn t1) [] t1
   where
    t1 = evalState (instantiateVars [] env t) (0 :: Int)

buildTypeConstructorEnvironment :: [Definition a k t] -> TypeConstructorEnvironment
buildTypeConstructorEnvironment =
  makeEnv
    ( \case
        DType name ps _ ->
          [
            ( name
            , foldr KArrow KType (replicate (length ps) KType)
            )
          ]
        _ -> []
    )

buildTraitEnvironment :: TypeConstructorEnvironment -> [Definition a k t] -> TraitEnvironment
buildTraitEnvironment env =
  makeEnv
    ( \case
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
    )

buildAliasEnvironment :: [Definition a k t] -> AliasEnvironment
buildAliasEnvironment =
  makeEnv
    ( \case
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
    )

buildInstanceEnvironment :: TypeConstructorEnvironment -> TraitEnvironment -> [Definition a k t] -> InstanceEnvironment
buildInstanceEnvironment env1 env2 ds = execState (traverse_ go ds) mempty
 where
  go =
    \case
      DInstance name t _ ->
        case Environment.lookup name env2 of
          Just (TypeIndex{..}, env3) -> do
            modify (Environment.insertWith Map.union name val)
           where
            val = Map.singleton t1 (Map.fromList (substituteInScheme (typeIndexId `mapsTo` t1) <$$> fs))
            fs = Environment.toList env3
            t1 = evalState (instantiateVars [] env1 t) (freshId fs)
            freshId = freshIdIn . indexSet . fmap snd
          Nothing ->
            error ("Trait '" <> Text.unpack name <> "' not in scope.")
      _ ->
        pure ()

indexSet :: [IndexedScheme] -> Set (TypeIndex Kind)
indexSet = Set.unions . fmap vars where vars (Forall vs _ _) = vs
