{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Environment (
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

import Debug.Trace
import Coal.Common.Environment (Environment (..))
import Coal.Compiler.Transform.Type.AliasExpansion
import Coal.Compiler.Transform.Type.Parameterized
import Coal.Language
import Coal.Language.Module.Definition
import Coal.TypeSystem.Substitution (apply, mapsTo, substituteInScheme)
import Control.Monad.State (evalState, execState, modify)
import Control.Monad.Writer (execWriterT)
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Extra (Dictionary, Name, Set, traverse_, (<$$>), traverse2)
import Data.List (nub)

import qualified Coal.Common.Environment as Environment
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

type DataConstructorEnvironment = Environment (Constructor TypeIndex Kind IndexedType)
type TypeConstructorEnvironment = Environment Kind
type TraitEnvironment = Environment (Parameter Kind, TypeIndex Kind, Environment IndexedScheme)
type InstanceEnvironment = Environment (Map IndexedType (Type Parameter (), Dictionary IndexedScheme))
type CodataAccessorEnvironment = Environment (CodataAccessor TypeIndex Kind IndexedType)

data CompilerEnvironment = CompilerEnvironment
  { compilerDataConstructorEnvironment :: DataConstructorEnvironment
  , compilerTypeConstructorEnvironment :: TypeConstructorEnvironment
  , compilerTraitEnvironment :: TraitEnvironment
  , compilerInstanceEnvironment :: InstanceEnvironment
  , compilerAliasEnvironment :: AliasEnvironment
  , compilerCodataAccessorEnvironment :: CodataAccessorEnvironment
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
    , compilerCodataAccessorEnvironment = mempty
    }

buildEnvironment :: [Definition a k t] -> CompilerEnvironment
buildEnvironment defs =
  CompilerEnvironment
    { compilerDataConstructorEnvironment = dataConstructorEnvironment
    , compilerTypeConstructorEnvironment = typeConstructorEnvironment
    , compilerTraitEnvironment = traitEnvironment
    , compilerInstanceEnvironment = instanceEnvironment
    , compilerAliasEnvironment = aliasEnvironment
    , compilerCodataAccessorEnvironment = codataAccessorEnvironment
    }
 where
  instanceEnvironment = buildInstanceEnvironment typeConstructorEnvironment traitEnvironment defs
  aliasEnvironment = buildAliasEnvironment defs
  traitEnvironment = buildTraitEnvironment typeConstructorEnvironment defs
  dataConstructorEnvironment = buildDataConstructorEnvironment typeConstructorEnvironment defs
  typeConstructorEnvironment = buildTypeConstructorEnvironment defs
  codataAccessorEnvironment = buildCodataAccessorEnvironment defs

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
            , (Parameter k n, TypeIndex k 0, Environment.fromList (f <$$> ds))
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
      DInstance name ts t _ ->
        case Environment.lookup name env2 of
          Just (p1, TypeIndex{..}, env3) -> do
            let (t1t1, tsts) = evalState bork (freshId fs)
                sub = typeIndexId `mapsTo` t1t1
                mppp = Map.fromList (insertTraits tsts . substituteInScheme sub <$$> fs)
                val = Map.singleton t1t1 (t, mppp)
            modify (Environment.insertWith Map.union name val)
           where
            bork = do 
                ts1 <- execWriterT (instantiateTypeIndexes t) 
                let env4 = Environment.insert (parameterName p1) (TypeIndex (parameterKind p1) typeIndexId) (Environment.fromList ts1)
                flip runReaderT (env4, env1) $ do
                   aa <- instantiateTypeVars t
                   bb <- traverse2 instantiateTypeVars ts
                   pure (aa, nub bb)
            fs = Environment.toList env3
            freshId = freshIdIn . indexSet . fmap snd
          Nothing ->
            error ("Trait '" <> Text.unpack name <> "' not in scope.")
      _ ->
        pure ()

-- TODO
insertTraits ts (Forall ds _ s) = Forall ds ts (foldType s xs)
  where
    xs :: [IndexedType]
    xs = fmap typeOf ts

buildCodataAccessorEnvironment :: [Definition a k t] -> CodataAccessorEnvironment
buildCodataAccessorEnvironment defs = mempty -- TODO:

indexSet :: [IndexedScheme] -> Set (TypeIndex Kind)
indexSet = Set.unions . fmap vars where vars (Forall vs _ _) = vs
