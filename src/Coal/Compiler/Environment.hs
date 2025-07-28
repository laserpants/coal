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

import Coal.Common.Environment (Environment (..))
import Coal.Compiler.Transform.Type.AliasExpansion
import Coal.Compiler.Transform.Type.Parameterized
import Coal.Language
import Coal.Common.List1 (NonEmpty (..))
import Coal.Language.Module.Definition
import Data.Maybe (fromMaybe)
import Data.List ((\\))
import Coal.TypeSystem.Substitution (mapsTo, substituteInScheme)
import Control.Monad.State (evalState, execState, modify)
import Data.Map.Strict (Map)
import Extra (Dictionary, Name, Set, traverse_, (<$$>), isConstructor)

import qualified Coal.Common.Environment as Environment
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

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
  dataConstructorEnvironment = buildDataConstructorEnvironment testEnv1 typeConstructorEnvironment defs
  typeConstructorEnvironment = buildTypeConstructorEnvironment testEnv2 defs

testEnv1 =
  Map.fromList
    [
      ( "Tree"
      , Environment.fromList
          [ 
            ( "Node"
            , Constructor
                "Node"
                3
                (Forall (Set.fromList [TypeIndex KType 0]) 
                    [] 
                    (TVariable (TypeIndex KType 0)
                        `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])
                        `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])
                        `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])
                    )
                )
            )
          , ( "Leaf"
            , Constructor
                "Leaf"
                0
                (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])))
            )
          ]
      )
    ]

testEnv2 =
  Map.fromList
    [
      ( "Tree"
      , Environment.fromList
          [ 
            ( "Tree"
            , KType `KArrow` KType
            )
          ]
      )
    ]

makeEnv :: (Definition a k t -> [(Name, e)]) -> [Definition a k t] -> Environment e
makeEnv f = Environment.fromList . concatMap f

findAll :: [Name] -> Environment a -> Either [Name] [(Name, a)]
findAll names env = 
    if length names == length found
      then Right found
      else Left (names \\ (fst <$> found))
  where
    found =
      Environment.lookupAll names env

buildDataConstructorEnvironment :: Dictionary DataConstructorEnvironment -> TypeConstructorEnvironment -> [Definition a k t] -> DataConstructorEnvironment
buildDataConstructorEnvironment de env =
  makeEnv
    ( \case
        DType _ _ cs ->
          translateConstructor <$> cs
        DImport (Path pp) ns -> do
          let cs = filter isConstructor ns
              pn = Text.intercalate "." pp
              env1 = fromMaybe mempty (Map.lookup pn de)
          case findAll cs env1 of
            Left missing ->
              error (show missing)
            Right found ->
              found
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

buildTypeConstructorEnvironment :: Dictionary TypeConstructorEnvironment -> [Definition a k t] -> TypeConstructorEnvironment
buildTypeConstructorEnvironment te =
  makeEnv
    ( \case
        DType name ps _ ->
          [
            ( name
            , foldr KArrow KType (replicate (length ps) KType)
            )
          ]
        DImport (Path pp) ns -> do
          let cs = filter isConstructor ns
              pn = Text.intercalate "." pp
              env1 = fromMaybe mempty (Map.lookup pn te)
          case findAll cs env1 of
            Left missing ->
              error (show missing)
            Right found ->
              found
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
