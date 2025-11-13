{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Module.Builders where

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

    unless (["*"] == exports) $ do
      defs <- gets moduleExports
      modify $ setExports (exports `union` Set.toList defs)
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
            insertInstance trait t1 (instanceInfo loc es def)
         where
          t1 = toIndexedType kinds p q
          es = Environment.mapEnvironment (substituteInScheme (0 `mapsTo` t1) . toIndexedScheme kinds p) dict
    _ ->
      pure ()

substituteInScheme :: Substitution -> Scheme o Kind IndexedType -> IndexedScheme
substituteInScheme sub (Forall _ ts t) = scheme (apply sub ts) (apply sub t)
