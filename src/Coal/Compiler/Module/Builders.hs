{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Module.Builders (banan3, banan2, build, typeConstructorEnv) where

import Coal.Compiler.Journal

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Module.Bundle
import Coal.Compiler.Stack
import Coal.Compiler.State (overCompilerModules)
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Substitution
import Control.Monad.Except
import Control.Monad.State (StateT, execStateT, gets, modify)
import Data.List (union, (\\))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Extras (Name)

banan3 :: (Monad m) => CompilerT a m (Environment IndexedScheme)
banan3 = do
  path <- gets (principalPath . compilerCurrentModule)
  modules <- gets compilerModules
  case Environment.lookup path modules of
    Nothing ->
      -- TODO
      throwError PreflightFailure
    Just ModuleBundle{..} -> do
      flip execStateT mempty $ do
        forM_ (Environment.toList moduleNames) $
          \case
            (name, IFunction s) ->
              modify (Environment.insert name s)
            (name, IConstant s) ->
              modify (Environment.insert name s)
            (name, IFold s) ->
              modify (Environment.insert name s)
            (name, IUnfold s) ->
              modify (Environment.insert name s)
            _ ->
              pure ()

banan2 :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
banan2 store = do
  -- TODO: DRY
  path <- gets (principalPath . compilerCurrentModule)
  modules <- gets compilerModules
  case Environment.lookup path modules of
    Nothing ->
      -- TODO
      throwError PreflightFailure
    Just bundle -> do
      updatedBundle <- banan bundle
      modify (overCompilerModules (Environment.insert path updatedBundle))
 where
  banan :: (Monad m) => ModuleBundle a -> CompilerT a m (ModuleBundle a)
  banan bundle@ModuleBundle{..} = do
    flip execStateT bundle $ do
      forM_ (Environment.toList moduleNames) $
        \case
          (name, IFunctionPlaceholder) ->
            go name IFunction
          (name, IConstantPlaceholder) ->
            go name IConstant
          (name, IFoldPlaceholder) ->
            go name IFold
          (name, IUnfoldPlaceholder) ->
            go name IUnfold
          _ ->
            pure ()
  go :: (Monad m) => Name -> (IndexedScheme -> NameInfo) -> StateT (ModuleBundle a) (CompilerT a m) ()
  go name info =
    case Environment.lookup name store of
      Nothing ->
        error ("No name: " <> show name)
      Just s ->
        modify $ addName name (info s)

build :: (Monad m, Monoid a) => Module a Kind () -> CompilerT a m (ModuleBundle a)
build (Module path exports defs) =
  flip execStateT emptyModuleBundle $ do
    modify (setPath path)

    inEachDef collectTypeConstructors

    modify $
      insertTypeConstructor "List" (TypeConstructorInfo mempty "List" (KArrow KType KType))
        . addName "List" (IType (KArrow KType KType))

    kinds <- typeConstructorEnv
    inEachDef (collectDataConstructors kinds)

    modify $
      insertDataConstructor
        "Zero"
        ( DataConstructorInfo
            mempty
            "Zero"
            ( DataConstructor
                "Zero"
                0
                (Forall mempty [] (TIntrinsic INat))
            )
            (Set.fromList ["Succ", "Zero"])
        )
        . addName "Zero" (IDataConstructor (Forall mempty [] (TIntrinsic INat)))

    modify $
      insertDataConstructor
        "Succ"
        ( DataConstructorInfo
            mempty
            "Succ"
            ( DataConstructor
                "Succ"
                1
                (Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat))
            )
            (Set.fromList ["Succ", "Succ"])
        )
        . addName "Succ" (IDataConstructor (Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat)))

    inEachDef (collectTraits kinds)
    traits <- traitEnv
    inEachDef (collectInstances kinds traits)

    modify $
      insertInstance
        "Numeric"
        (TIntrinsic IInt32)
        ( InstanceInfo
            mempty
            (TIntrinsic IInt32)
            (TIntrinsic IInt32)
            ( Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                  )
                ,
                  ( "negate"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                  )
                ,
                  ( "(+)"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                  )
                ,
                  ( "(-)"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                  )
                ,
                  ( "(*)"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Numeric"
        (TIntrinsic IInt64)
        ( InstanceInfo
            mempty
            (TIntrinsic IInt64)
            (TIntrinsic IInt64)
            ( Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt64)
                  )
                ,
                  ( "negate"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
                  )
                ,
                  ( "(+)"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
                  )
                ,
                  ( "(-)"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
                  )
                ,
                  ( "(*)"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Numeric"
        (TIntrinsic IFloat)
        ( InstanceInfo
            mempty
            (TIntrinsic IFloat)
            (TIntrinsic IFloat)
            ( Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IFloat)
                  )
                ,
                  ( "negate"
                  , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat)
                  )
                ,
                  ( "(+)"
                  , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IFloat)
                  )
                ,
                  ( "(-)"
                  , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IFloat)
                  )
                ,
                  ( "(*)"
                  , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IFloat)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Numeric"
        (TIntrinsic IDouble)
        ( InstanceInfo
            mempty
            (TIntrinsic IDouble)
            (TIntrinsic IDouble)
            ( Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IDouble)
                  )
                ,
                  ( "negate"
                  , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble)
                  )
                ,
                  ( "(+)"
                  , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IDouble)
                  )
                ,
                  ( "(-)"
                  , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IDouble)
                  )
                ,
                  ( "(*)"
                  , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IDouble)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Numeric"
        (TIntrinsic INat)
        ( InstanceInfo
            mempty
            (TIntrinsic INat)
            (TIntrinsic INat)
            ( Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic INat)
                  )
                ,
                  ( "negate"
                  , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat)
                  )
                ,
                  ( "(+)"
                  , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TIntrinsic INat)
                  )
                ,
                  ( "(-)"
                  , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TIntrinsic INat)
                  )
                ,
                  ( "(*)"
                  , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TIntrinsic INat)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Numeric"
        (TIntrinsic IBignum)
        ( InstanceInfo
            mempty
            (TIntrinsic IBignum)
            (TIntrinsic IBignum)
            ( Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IBignum)
                  )
                ,
                  ( "negate"
                  , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum)
                  )
                ,
                  ( "(+)"
                  , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TIntrinsic IBignum)
                  )
                ,
                  ( "(-)"
                  , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TIntrinsic IBignum)
                  )
                ,
                  ( "(*)"
                  , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TIntrinsic IBignum)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Ordered"
        (TIntrinsic IInt32)
        ( InstanceInfo
            mempty
            (TIntrinsic IInt32)
            (TIntrinsic IInt32)
            ( Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Ordered"
        (TIntrinsic IInt64)
        ( InstanceInfo
            mempty
            (TIntrinsic IInt64)
            (TIntrinsic IInt64)
            ( Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Ordered"
        (TIntrinsic IBool)
        ( InstanceInfo
            mempty
            (TIntrinsic IBool)
            (TIntrinsic IBool)
            ( Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Ordered"
        (TIntrinsic INat)
        ( InstanceInfo
            mempty
            (TIntrinsic INat)
            (TIntrinsic INat)
            ( Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Ordered"
        (TIntrinsic IFloat)
        ( InstanceInfo
            mempty
            (TIntrinsic IFloat)
            (TIntrinsic IFloat)
            ( Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Ordered"
        (TIntrinsic IDouble)
        ( InstanceInfo
            mempty
            (TIntrinsic IDouble)
            (TIntrinsic IDouble)
            ( Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Ordered"
        (TIntrinsic IChar)
        ( InstanceInfo
            mempty
            (TIntrinsic IChar)
            (TIntrinsic IChar)
            ( Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IChar `TArrow` TIntrinsic IChar `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Ordered"
        (TIntrinsic IBignum)
        ( InstanceInfo
            mempty
            (TIntrinsic IBignum)
            (TIntrinsic IBignum)
            ( Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Comparable"
        (TIntrinsic IInt32)
        ( InstanceInfo
            mempty
            (TIntrinsic IInt32)
            (TIntrinsic IInt32)
            ( Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Comparable"
        (TIntrinsic IInt64)
        ( InstanceInfo
            mempty
            (TIntrinsic IInt64)
            (TIntrinsic IInt64)
            ( Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IBool)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Comparable"
        (TIntrinsic IBool)
        ( InstanceInfo
            mempty
            (TIntrinsic IBool)
            (TIntrinsic IBool)
            ( Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Comparable"
        (TIntrinsic INat)
        ( InstanceInfo
            mempty
            (TIntrinsic INat)
            (TIntrinsic INat)
            ( Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TIntrinsic IBool)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Comparable"
        (TIntrinsic IFloat)
        ( InstanceInfo
            mempty
            (TIntrinsic IFloat)
            (TIntrinsic IFloat)
            ( Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IBool)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Comparable"
        (TIntrinsic IDouble)
        ( InstanceInfo
            mempty
            (TIntrinsic IDouble)
            (TIntrinsic IDouble)
            ( Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IBool)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Comparable"
        (TIntrinsic IChar)
        ( InstanceInfo
            mempty
            (TIntrinsic IChar)
            (TIntrinsic IChar)
            ( Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IChar `TArrow` TIntrinsic IChar `TArrow` TIntrinsic IBool)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Comparable"
        (TIntrinsic IBignum)
        ( InstanceInfo
            mempty
            (TIntrinsic IBignum)
            (TIntrinsic IBignum)
            ( Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TIntrinsic IBool)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Divisible"
        (TIntrinsic IFloat)
        ( InstanceInfo
            mempty
            (TIntrinsic IFloat)
            (TIntrinsic IFloat)
            ( Map.fromList
                [
                  ( "(/)"
                  , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IInt32)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Divisible"
        (TIntrinsic IDouble)
        ( InstanceInfo
            mempty
            (TIntrinsic IDouble)
            (TIntrinsic IDouble)
            ( Map.fromList
                [
                  ( "(/)"
                  , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IInt32)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Modulo"
        (TIntrinsic IInt32)
        ( InstanceInfo
            mempty
            (TIntrinsic IInt32)
            (TIntrinsic IInt32)
            ( Map.fromList
                [
                  ( "(%)"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Modulo"
        (TIntrinsic IInt64)
        ( InstanceInfo
            mempty
            (TIntrinsic IInt64)
            (TIntrinsic IInt64)
            ( Map.fromList
                [
                  ( "(%)"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Semigroup"
        (TIntrinsic IString)
        ( InstanceInfo
            mempty
            (TIntrinsic IString)
            (TIntrinsic IString)
            ( Map.fromList
                [
                  ( "(<>)"
                  , Forall mempty [] (TIntrinsic IString `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString)
                  )
                ]
            )
        )

    modify $
      insertInstance
        "Semigroup"
        (TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| []))
        ( InstanceInfo
            mempty
            (TApplication () (TConstructor () "List") (TVariable (Parameter () "a") :| []))
            (TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| []))
            ( Map.fromList
                [
                  ( "(<>)"
                  , Forall
                      mempty
                      []
                      ( TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
                          `TArrow` TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
                          `TArrow` TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
                      )
                  )
                ]
            )
        )

    inEachDef collectImportedNames
    inEachDef collectPlaceholders

    let builtin = Set.fromList ["(%)", "(*)", "(+)", "(-)", "(/)", "(<>)", "(==)", "Comparable", "Divisible", "EqualTo", "GreaterThan", "IO", "LessThan", "Modulo", "None", "Numeric", "Option", "Ordered", "Ordering", "Semigroup", "Some", "compare", "from_int32", "negate"]

    exps <- gets (Set.filter (`notElem` builtin) . moduleExports)
    unless (["*"] == exports) $ do
      modify $ setExports (exports `union` Set.toList exps)
 where
  inEachDef = forM_ defs

pick :: [Name] -> Environment a -> [(Name, a)]
pick names = Environment.toList . Environment.restrict names

collectNames :: (Monad m) => Definition a Kind () -> (ModuleBundle a -> Environment e) -> StateT (ModuleBundle a) (CompilerT a m) ([(Name, e)], [Name])
collectNames (DImport _ (Path ["Builtin$"]) _) _ =
  pure ([], [])
collectNames (DImport _ path names) getter = do
  bundle <- importedModule path
  let env = getter bundle
  pure (pick names env, names \\ Environment.names env)
collectNames _ _ = error "Implementation error"

collect :: (Monad m) => Definition a Kind () -> (ModuleBundle a -> Environment e) -> StateT (ModuleBundle a) (CompilerT a m) [e]
collect d f = fmap snd . fst <$> collectNames d f

importedModule :: (Monad m) => Path -> StateT (ModuleBundle a) (CompilerT a m) (ModuleBundle a)
importedModule path = do
  env <- lift (gets compilerModules)
  case Environment.lookup (principalPath path) env of
    Nothing ->
      -- TODO: No such module
      error ("No module: " <> show path)
    Just bundle -> do
      return bundle

collectTypeConstructors :: (Monad m) => Definition a Kind () -> StateT (ModuleBundle a) (CompilerT a m) ()
collectTypeConstructors =
  \case
    DType loc name def -> do
      modify $
        insertTypeConstructor name info
          . addName name (IType kind_)
          . addExport name
     where
      info@(TypeConstructorInfo _ _ kind_) = typeConstructorInfo loc name def
    DCotype loc name def -> do
      modify $
        insertCotypeConstructor name info
          . addName name (ICotype kind_)
          . addExport name
     where
      info@(CotypeConstructorInfo _ _ kind_) = cotypeConstructorInfo loc name def
    DTypeAlias loc name alias -> do
      modify $
        insertAlias name (aliasInfo loc name alias)
          . addName name IAlias
          . addExport name
    def@DImport{} -> do
      types <- collect def exportedTypeConstructors
      forM_ types $
        \info@(TypeConstructorInfo _ name _) ->
          modify $ insertTypeConstructor name info
      cotypes <- collect def exportedCotypeConstructors
      forM_ cotypes $
        \info@(CotypeConstructorInfo _ name _) ->
          modify $ insertCotypeConstructor name info
    _ ->
      pure ()

{-# INLINE foldElems #-}
foldElems :: (Monoid m) => (a -> m -> m) -> Environment a -> m
foldElems f = foldr f mempty . Environment.elems

traitEnv :: (Monad m) => StateT (ModuleBundle a) (CompilerT a m) (Environment (TraitInfo a))
traitEnv = do
  gets (foldElems insertTraitInfo . moduleTraits)
 where
  insertTraitInfo :: TraitInfo a -> Environment (TraitInfo a) -> Environment (TraitInfo a)
  insertTraitInfo info@(TraitInfo _ name _ _) = Environment.insert name info

typeConstructorEnv :: (Monad m) => StateT (ModuleBundle a) (CompilerT a m) (Environment Kind)
typeConstructorEnv = do
  env1 <- gets (foldElems insertTypeInfo . moduleTypeConstructors)
  env2 <- gets (foldElems insertCotypeInfo . moduleCotypeConstructors)
  pure (env1 <> env2)
 where
  insertTypeInfo :: TypeConstructorInfo a -> Environment Kind -> Environment Kind
  insertTypeInfo (TypeConstructorInfo _ name kind_) = Environment.insert name kind_

  insertCotypeInfo :: CotypeConstructorInfo a -> Environment Kind -> Environment Kind
  insertCotypeInfo (CotypeConstructorInfo _ name kind_) = Environment.insert name kind_

collectDataConstructors :: (Monad m) => Environment Kind -> Definition a Kind () -> StateT (ModuleBundle a) (CompilerT a m) ()
collectDataConstructors env =
  \case
    DCotype loc _ def ->
      forM_ (codataAccessorInfo env loc def) $
        \info@(CodataAccessorInfo _ _ CodataAccessor{..}) -> do
          modify $
            addName codataAccessorName (ICodataAccessor codataAccessorScheme)
              . insertCodataAccessor codataAccessorName info
              . addExport codataAccessorName
    DType loc _ def ->
      forM_ (dataConstructorInfo env loc def) $
        \info@(DataConstructorInfo _ _ DataConstructor{..} _) -> do
          modify $
            addName constructorName (IDataConstructor constructorScheme)
              . insertDataConstructor constructorName info
              . addExport constructorName
    def@DImport{} -> do
      ctors <- collect def exportedDataConstructors
      forM_ ctors $
        \info@(DataConstructorInfo _ _ DataConstructor{..} _) ->
          modify $ insertDataConstructor constructorName info
      xsors <- collect def exportedCodataAccessors
      forM_ xsors $
        \info@(CodataAccessorInfo _ _ CodataAccessor{..}) ->
          modify $ insertCodataAccessor codataAccessorName info
    _ ->
      pure ()

collectTraits :: (Monad m) => Environment Kind -> Definition a Kind () -> StateT (ModuleBundle a) (CompilerT a m) ()
collectTraits env =
  \case
    DTrait loc name def -> do
      addTraitEntries env name def
      modify $
        addName name ITrait
          . insertTrait name (traitInfo loc name def)
          . addExport name
    _ ->
      pure ()

addTraitEntries :: (Monad m) => Environment Kind -> Name -> TraitDef () -> StateT (ModuleBundle a) (CompilerT a m) ()
addTraitEntries env trait (TraitDef _ p entries) =
  forM_ entries $
    -- TODO
    \(name, Forall _ _ t) ->
      modify $
        addName name (IFunction $ scheme [Trait trait tvar] (toIndexedType env p t))
          . addExport name
 where
  tvar = TVariable (TypeIndex (parameterKind p) 0)

collectInstances :: (Monad m) => Environment Kind -> Environment (TraitInfo a) -> Definition a Kind () -> StateT (ModuleBundle a) (CompilerT a m) ()
collectInstances kinds traits =
  \case
    DInstance loc trait (InstanceDef _ q _) ->
      case Environment.lookup trait traits of
        Nothing ->
          -- TODO
          error "Trait not in scope!"
        Just (TraitInfo _ _ p dict) -> do
          modify $ insertInstance trait t1 (InstanceInfo loc q (toIndexedType kinds p q) env)
         where
          t1 = toIndexedType kinds p q
          Environment env = Environment.mapEnvironment (substituteInScheme (0 `mapsTo` t1) . toIndexedScheme kinds p) dict
    _ ->
      pure ()

substituteInScheme :: Substitution -> Scheme o Kind IndexedType -> IndexedScheme
substituteInScheme sub (Forall _ ts t) = scheme (apply sub ts) (apply sub t)

collectImportedNames :: (Monad m) => Definition a Kind () -> StateT (ModuleBundle a) (CompilerT a m) ()
collectImportedNames =
  \case
    def@(DImport loc module_ _) -> do
      (names, missing) <- collectNames def exportedNames
      unless (null missing) $ do
        path <- lift (gets (principalPath . compilerCurrentModule))
        forM_ missing $
          \name ->
            tellErrors [NameNotInModule name (principalPath module_) (ErrorLocation path loc)]
        throwError PreflightFailure
      forM_ names $
        \case
          (_, IFunctionPlaceholder) ->
            pure ()
          (_, IConstantPlaceholder) ->
            pure ()
          (_, IFoldPlaceholder) ->
            pure ()
          (_, IUnfoldPlaceholder) ->
            pure ()
          (name, info) ->
            modify $ addName name info
    _ ->
      pure ()

collectPlaceholders :: (Monad m) => Definition a Kind () -> StateT (ModuleBundle a) (CompilerT a m) ()
collectPlaceholders =
  \case
    DFunction _ name _ _ ->
      modify $
        addName name IFunctionPlaceholder
          . addExport name
    DConstant _ name _ _ ->
      modify $
        addName name IConstantPlaceholder
          . addExport name
    DFold _ name _ ->
      modify $
        addName name IFoldPlaceholder
          . addExport name
    DUnfold _ name _ ->
      modify $
        addName name IUnfoldPlaceholder
          . addExport name
    _ ->
      pure ()
