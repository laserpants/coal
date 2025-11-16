{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Module.Builders (build, typeConstructorEnv) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Module.Bundle
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Substitution
import Control.Monad (unless)
import Control.Monad.State (StateT, execStateT, gets, lift, modify)
import Data.List (union)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Extras (Name, forM_)

build :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m (ModuleBundle Metadata)
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

    unless (["*"] == exports) $ do
      exps <- gets moduleExports
      modify $ setExports (exports `union` Set.toList exps)
 where
  inEachDef = forM_ defs

pick :: [Name] -> Environment a -> [a]
pick names = Environment.elems . Environment.restrict names

collect :: (Monad m) => Definition Metadata Kind () -> (ModuleBundle Metadata -> Environment a) -> StateT (ModuleBundle Metadata) (CompilerT Metadata m) [a]
-- TODO
collect (DImport _ (Path ["Builtin$"]) _) _ = do
  pure []
collect (DImport _ path names) getter = do
  bundle <- importedModule path
  pure $ pick names (getter bundle)
collect _ _ = error "Implementation error"

importedModule :: (Monad m) => Path -> StateT (ModuleBundle Metadata) (CompilerT Metadata m) (ModuleBundle Metadata)
importedModule path = do
  env <- lift (gets compilerModules)
  case Environment.lookup (principalPath path) env of
    Nothing ->
      -- TODO: No such module
      error ("No module: " <> show path)
    Just bundle -> do
      return bundle

collectTypeConstructors :: (Monad m) => Definition Metadata Kind () -> StateT (ModuleBundle Metadata) (CompilerT Metadata m) ()
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
        \info@(TypeConstructorInfo _ name kind_) ->
          modify $
            insertTypeConstructor name info . addName name (IType kind_)
      cotypes <- collect def exportedCotypeConstructors
      forM_ cotypes $
        \info@(CotypeConstructorInfo _ name kind_) ->
          modify $
            insertCotypeConstructor name info . addName name (ICotype kind_)
    _ ->
      pure ()

{-# INLINE foldElems #-}
foldElems :: (Monoid m) => (a -> m -> m) -> Environment a -> m
foldElems f = foldr f mempty . Environment.elems

traitEnv :: (Monad m) => StateT (ModuleBundle Metadata) (CompilerT Metadata m) (Environment (TraitInfo Metadata))
traitEnv = do
  gets (foldElems insertTraitInfo . moduleTraits)
 where
  insertTraitInfo :: TraitInfo Metadata -> Environment (TraitInfo Metadata) -> Environment (TraitInfo Metadata)
  insertTraitInfo info@(TraitInfo _ name _ _) = Environment.insert name info

typeConstructorEnv :: (Monad m) => StateT (ModuleBundle Metadata) (CompilerT Metadata m) (Environment Kind)
typeConstructorEnv = do
  env1 <- gets (foldElems insertTypeInfo . moduleTypeConstructors)
  env2 <- gets (foldElems insertCotypeInfo . moduleCotypeConstructors)
  pure (env1 <> env2)
 where
  insertTypeInfo :: TypeConstructorInfo Metadata -> Environment Kind -> Environment Kind
  insertTypeInfo (TypeConstructorInfo _ name kind_) = Environment.insert name kind_

  insertCotypeInfo :: CotypeConstructorInfo Metadata -> Environment Kind -> Environment Kind
  insertCotypeInfo (CotypeConstructorInfo _ name kind_) = Environment.insert name kind_

collectDataConstructors :: (Monad m) => Environment Kind -> Definition Metadata Kind () -> StateT (ModuleBundle Metadata) (CompilerT Metadata m) ()
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
          modify $
            addName constructorName (IDataConstructor constructorScheme)
              . insertDataConstructor constructorName info
      xsors <- collect def exportedCodataAccessors
      forM_ xsors $
        \info@(CodataAccessorInfo _ _ CodataAccessor{..}) ->
          modify $
            addName codataAccessorName (ICodataAccessor codataAccessorScheme)
              . insertCodataAccessor codataAccessorName info
    _ ->
      pure ()

collectTraits :: (Monad m) => Environment Kind -> Definition Metadata Kind () -> StateT (ModuleBundle Metadata) (CompilerT Metadata m) ()
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

addTraitEntries :: (Monad m) => Environment Kind -> Name -> TraitDef () -> StateT (ModuleBundle Metadata) (CompilerT Metadata m) ()
addTraitEntries env trait (TraitDef _ p entries) =
  forM_ entries $
    -- TODO
    \(name, Forall _ _ t) ->
      modify $
        addName name (IFunction $ scheme [Trait trait tvar] (toIndexedType env p t))
          . addExport name
 where
  tvar = TVariable (TypeIndex (parameterKind p) 0)

collectInstances :: (Monad m) => Environment Kind -> Environment (TraitInfo Metadata) -> Definition Metadata Kind () -> StateT (ModuleBundle Metadata) (CompilerT Metadata m) ()
collectInstances kinds traits =
  \case
    DInstance loc trait def@(InstanceDef _ q _) ->
      case Environment.lookup trait traits of
        Nothing ->
          -- TODO
          error "Trait not in scope!"
        Just (TraitInfo _ _ p dict) -> do
          modify $
            insertInstance trait t1 (instanceInfo loc p kinds es def)
         where
          t1 = toIndexedType kinds p q
          es = Environment.mapEnvironment (substituteInScheme (0 `mapsTo` t1) . toIndexedScheme kinds p) dict
    _ ->
      pure ()

substituteInScheme :: Substitution -> Scheme o Kind IndexedType -> IndexedScheme
substituteInScheme sub (Forall _ ts t) = scheme (apply sub ts) (apply sub t)
