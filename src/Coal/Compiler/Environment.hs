{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Environment (
  AliasEnvironment,
  DataConstructorEnvironment,
  TypeConstructorEnvironment,
  TraitEnvironment,
  InstanceEnvironment,
  CompilerEnvironment (..),
  KernelEnvironment (..),
  emptyCompilerEnvironment,
  buildEnvironment,
  buildAliasEnvironment,
  buildInstanceEnvironment,
  overCompilerDictionaryNameEnvironment,
  overCompilerKernelEnvironment,
  overKernelEnvironmentModule,
  overKernelEnvironmentLocalNames,
  overKernelEnvironmentQualifiedNames,
  insertEnv,
) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Builtin.Environment
import Coal.Compiler.Module.Bundle
import Coal.Compiler.Transform.Type.Parameterized
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Substitution
import Control.Monad.Reader
import Control.Monad.State (evalState, execState, modify)
import Control.Monad.Writer (execWriterT)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Extras (Dictionary, Name, Over, Set, traverse2, traverse_, (<$$>))

type AliasEnvironment = Environment (AliasInfo Metadata) -- ([Name], ParameterizedType)
type DataConstructorEnvironment = Environment (DataConstructorInfo Metadata)
type TypeConstructorEnvironment = Environment Kind
type TraitEnvironment = Environment (TraitInfo Metadata) -- (Parameter Kind, TypeIndex Kind, Environment IndexedScheme)
type InstanceEnvironment = Environment (Map IndexedType (InstanceInfo Metadata))
type CodataAccessorEnvironment = Environment (CodataAccessorInfo Metadata)
type FoldEnvironment = Environment IndexedScheme
type UnfoldEnvironment = Environment IndexedScheme
type DictionaryNameEnvironment = Environment IndexedScheme

data KernelEnvironment = KernelEnvironment
  { kernelEnvironmentModule :: Name
  , kernelEnvironmentLocalNames :: Set Name
  , kernelEnvironmentQualifiedNames :: Environment Name
  }
  deriving (Show, Eq, Ord, Read)

overKernelEnvironmentModule :: Over KernelEnvironment Name
overKernelEnvironmentModule fn KernelEnvironment{..} =
  KernelEnvironment{kernelEnvironmentModule = fn kernelEnvironmentModule, ..}

overKernelEnvironmentLocalNames :: Over KernelEnvironment (Set Name)
overKernelEnvironmentLocalNames fn KernelEnvironment{..} =
  KernelEnvironment{kernelEnvironmentLocalNames = fn kernelEnvironmentLocalNames, ..}

overKernelEnvironmentQualifiedNames :: Over KernelEnvironment (Environment Name)
overKernelEnvironmentQualifiedNames fn KernelEnvironment{..} =
  KernelEnvironment{kernelEnvironmentQualifiedNames = fn kernelEnvironmentQualifiedNames, ..}

data CompilerEnvironment = CompilerEnvironment
  { compilerDataConstructorEnvironment :: DataConstructorEnvironment
  , compilerTypeConstructorEnvironment :: TypeConstructorEnvironment
  , compilerTraitEnvironment :: TraitEnvironment
  , compilerInstanceEnvironment :: InstanceEnvironment
  , compilerAliasEnvironment :: AliasEnvironment
  , compilerCodataAccessorEnvironment :: CodataAccessorEnvironment
  , compilerDictionaryNameEnvironment :: DictionaryNameEnvironment
  , compilerKernelEnvironment :: KernelEnvironment
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
    , compilerDictionaryNameEnvironment = mempty
    , compilerKernelEnvironment = KernelEnvironment mempty mempty mempty
    }

overCompilerDictionaryNameEnvironment ::
  ( Environment IndexedScheme ->
    Environment IndexedScheme
  ) ->
  CompilerEnvironment ->
  CompilerEnvironment
overCompilerDictionaryNameEnvironment f CompilerEnvironment{..} =
  CompilerEnvironment
    { compilerDictionaryNameEnvironment =
        f compilerDictionaryNameEnvironment
    , ..
    }

overCompilerKernelEnvironment ::
  ( KernelEnvironment ->
    KernelEnvironment
  ) ->
  CompilerEnvironment ->
  CompilerEnvironment
overCompilerKernelEnvironment f CompilerEnvironment{..} =
  CompilerEnvironment
    { compilerKernelEnvironment =
        f compilerKernelEnvironment
    , ..
    }

buildEnvironment :: [Definition a k t] -> CompilerEnvironment
buildEnvironment defs =
  CompilerEnvironment
    { compilerDataConstructorEnvironment = mempty
    , compilerTypeConstructorEnvironment = mempty
    , compilerTraitEnvironment = traitEnvironment
    , compilerInstanceEnvironment = instanceEnvironment
    , compilerAliasEnvironment = mempty -- aliasEnvironment
    , compilerCodataAccessorEnvironment = codataAccessorEnvironment
    , compilerDictionaryNameEnvironment = dictionaryNameEnvironment
    , compilerKernelEnvironment = kernelEnvironment
    }
 where
  instanceEnvironment = buildInstanceEnvironment typeConstructorEnvironment traitEnvironment defs
  traitEnvironment = buildTraitEnvironment typeConstructorEnvironment defs
  typeConstructorEnvironment = buildTypeConstructorEnvironment defs
  codataAccessorEnvironment = buildCodataAccessorEnvironment typeConstructorEnvironment defs
  dictionaryNameEnvironment = mempty
  kernelEnvironment = KernelEnvironment mempty mempty mempty

makeEnv :: (Definition a k t -> [(Name, e)]) -> [Definition a k t] -> Environment e
makeEnv f = Environment.fromList . concatMap f

-- TODO
buildFoldEnvironment :: [Definition a k t] -> UnfoldEnvironment
buildFoldEnvironment _ = mempty

--    Environment.fromList
--      [
--        ( "encode_json_array"
--        , Forall
--            (Set.fromList mempty)
--            []
--            ( TApplication KType (TConstructor (KArrow KType KType) "List") (TConstructor KType "JsonValue" :| []) `TArrow` TIntrinsic IString
--            )
--        )
--      ,
--        ( "encode_json_object"
--        , Forall
--            (Set.fromList mempty)
--            []
--            ( TApplication KType (TConstructor (KArrow KType KType) "List") (tupleType (TIntrinsic IString :| [TConstructor KType "JsonValue"]) :| []) `TArrow` TIntrinsic IString
--            )
--        )
--      ,
--        ( "encode_json_value"
--        , Forall
--            (Set.fromList mempty)
--            []
--            ( TConstructor KType "JsonValue" `TArrow` TIntrinsic IString
--            )
--        )
--      ]

-- TODO
buildUnfoldEnvironment :: [Definition a k t] -> FoldEnvironment
buildUnfoldEnvironment _ = mempty

buildDataConstructorEnvironment :: TypeConstructorEnvironment -> [Definition a k t] -> DataConstructorEnvironment
buildDataConstructorEnvironment env =
  makeEnv
    ( \case
        DType _ _ (TypeDef _ cs) ->
          let ctors = Set.fromList (constructorName <$> cs)
           in undefined -- translateConstructor ctors <$> cs
        _ ->
          []
    )
 where
  --  translateConstructor ::
  --    Set Name ->
  --    DataConstructor a () ParameterizedType ->
  --    (Name, (DataConstructor TypeIndex Kind IndexedType, Set Name))
  translateConstructor ::
    Set Name ->
    DataConstructor a () ParameterizedType ->
    DataConstructorInfo Metadata
  translateConstructor ctors (DataConstructor n a s) =
    undefined

--    (n, (DataConstructor n a (translateScheme s), ctors))
--  translateScheme (Forall _ _ t) =
--    Forall (typeIndexesIn t1) [] t1
--   where
--    t1 = evalState (instantiateVars [] env t) (0 :: Int)

buildTypeConstructorEnvironment :: [Definition a k t] -> TypeConstructorEnvironment
buildTypeConstructorEnvironment =
  makeEnv
    ( \case
        DType _ name (TypeDef ps _) ->
          [
            ( name
            , foldr KArrow KType (replicate (length ps) KType)
            )
          ]
        DCotype _ name (CotypeDef ps _) ->
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
        -- DTrait _ name (TraitDef _ (Parameter k n) ds) ->
        --  [
        --    ( name
        --    ,
        --      ( Parameter k n
        --      , TypeIndex k 0
        --      , Environment.fromList (f <$$> ds)
        --      )
        --    )
        --  ]
        -- where
        --  f :: Scheme Parameter () ParameterizedType -> Scheme TypeIndex Kind IndexedType
        --  f (Forall _ _ t) =
        --    let t1 = evalState (instantiateVars [(n, TypeIndex k 0)] env t) (1 :: Int)
        --     in Forall (typeIndexesIn t1) [] t1
        _ ->
          []
    )

buildAliasEnvironment :: [Definition a k t] -> AliasEnvironment
buildAliasEnvironment =
  undefined

--  makeEnv
--    ( \case
--        DTypeAlias _ name (AliasDef ps t) ->
--          [
--            ( name
--            ,
--              ( parameterName <$> ps
--              , t
--              )
--            )
--          ]
--        _ ->
--          []
--    )

-- FIXME
buildInstanceEnvironment :: TypeConstructorEnvironment -> TraitEnvironment -> [Definition a k t] -> InstanceEnvironment
buildInstanceEnvironment ctorEnv traitEnv ds = execState (traverse_ go ds) mempty
 where
  go =
    \case
      --      DInstance _ name (InstanceDef ts t _) ->
      --        case Environment.lookup name traitEnv of
      --          Just (p1, TypeIndex{..}, sigs) -> do
      --            let (t1, _) = evalState f (freshId fs)
      --                sub = typeIndexId `mapsTo` t1
      --                -- map_ = Map.fromList (insertTraits builtinTraits . substituteInScheme sub <$$> fs)
      --                map_ = Map.fromList (substituteInScheme sub <$$> fs)
      --                val = Map.singleton t1 (t, map_)
      --            modify (Environment.insertWith Map.union name val)
      --           where
      --            f = do
      --              ts1 <- execWriterT (instantiateTypeIndexes t)
      --              let env = Environment.insert (parameterName p1) (TypeIndex (parameterKind p1) typeIndexId) (Environment.fromList ts1)
      --              flip runReaderT (env, ctorEnv) $ do
      --                y <- instantiateTypeVars t
      --                ys <- traverse2 instantiateTypeVars ts
      --                pure (y, nub ys)
      --            fs = Environment.toList sigs
      --            freshId = freshIdIn . indexSet . fmap snd
      --          Nothing ->
      --            error ("Trait '" <> Text.unpack name <> "' not in scope.")
      _ ->
        pure ()

substituteInScheme :: Substitution -> Scheme o Kind IndexedType -> IndexedScheme
substituteInScheme sub (Forall _ ts t) = scheme (apply sub ts) (apply sub t)

-- insertTraits :: (HasType o k (Trait (Type o k))) => [Trait (Type o k)] -> Scheme o k (Type o k) -> Scheme o k (Type o k)
-- insertTraits ts (Forall ds _ s) = Forall ds ts (foldTypeOf s ts)

pt (CodataAccessor _ (Forall _ _ t)) = t

buildCodataAccessorEnvironment :: TypeConstructorEnvironment -> [Definition a k t] -> CodataAccessorEnvironment
buildCodataAccessorEnvironment env =
  makeEnv
    ( \case
        --        DCotype _ name (CotypeDef ps ts) -> do
        --          let vs = traverse (instantiateVars params env . pt) ts
        --          [(n, accessor n t) | (n, t) <- (codataAccessorName <$> ts) `zip` evalState vs (freshIdIn ixs)]
        --         where
        --          accessor n t = CodataAccessor n (Forall (typeIndexesIn t) [] t)
        --          ixs = snd <$> params
        --          params = [(n, TypeIndex KType t) | (Parameter _ n, t) <- zip ps [0 ..]]
        _ ->
          []
    )

indexSet :: [IndexedScheme] -> Set (TypeIndex Kind)
indexSet = Set.unions . fmap vars where vars (Forall vs _ _) = vs

insertEnv :: Module Metadata Kind t -> CompilerEnvironment -> CompilerEnvironment
insertEnv (Module _ _ defs) = const $ insertBuiltInConstructors (buildEnvironment defs)

insertBuiltInConstructors :: CompilerEnvironment -> CompilerEnvironment
insertBuiltInConstructors CompilerEnvironment{..} =
  CompilerEnvironment
    { compilerDataConstructorEnvironment = Environment.insertMultiple builtinDataConstructors compilerDataConstructorEnvironment
    , compilerTraitEnvironment = Environment.insertMultiple builtinTraits compilerTraitEnvironment
    , compilerInstanceEnvironment = Environment.insertMultiple builtinInstances compilerInstanceEnvironment
    , compilerTypeConstructorEnvironment = Environment.insertMultiple builtinTypeConstructors compilerTypeConstructorEnvironment
    , compilerCodataAccessorEnvironment = Environment.insertMultiple builtinCodataAccessors compilerCodataAccessorEnvironment
    , ..
    }
