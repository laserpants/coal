{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Module.Builders where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Module.Bundle
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad.State (StateT, evalState, execStateT, gets, lift, modify)
import Data.List ((\\))
import Debug.Trace
import Extras (Name, forM_)

build :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m ModuleBundle
build (Module path exports defs) =
  flip execStateT emptyModuleBundle $ do
    modify $ setPath path . setExports exports
    inEachDef collectTypeConstructors
    kinds <- typeConstructorEnv
    inEachDef (collectDataConstructors kinds)
    inEachDef (collectTraits kinds)
 where
  --    traits <- traitEnv
  --    inEachDef (collectInstances kinds traits)

  inEachDef = forM_ defs

pick :: (Monad m) => [Name] -> Environment a -> StateT ModuleBundle (CompilerT Metadata m) [a]
pick names env
  | null missing = pure $ Environment.elems (Environment.restrict names env)
  | otherwise = error "TODO: Module ? doesn't export name ..."
 where
  missing = names \\ Environment.names env

collect :: (Monad m) => Definition Metadata Kind () -> (ModuleBundle -> Environment a) -> StateT ModuleBundle (CompilerT Metadata m) [a]
collect (DImport _ path names) getter = do
  bundle <- importedModule path
  pick names (getter bundle)
collect _ _ = error "Implementation error"

importedModule :: (Monad m) => Path -> StateT ModuleBundle (CompilerT Metadata m) ModuleBundle
importedModule path = do
  env <- lift (gets compilerModules)
  case Environment.lookup (principalPath path) env of
    Nothing ->
      -- TODO: No such module
      error ("No module: " <> show path)
    Just bundle -> do
      return bundle

collectTypeConstructors :: (Monad m) => Definition Metadata Kind () -> StateT ModuleBundle (CompilerT Metadata m) ()
collectTypeConstructors =
  \case
    DType loc name def -> do
      modify $
        insertTypeConstructor name info . addName name (IType kind_)
     where
      info@(TypeConstructorInfo _ _ kind_) = typeConstructorInfo loc name def
    DCotype loc name def -> do
      modify $
        insertCotypeConstructor name info . addName name (ICotype kind_)
     where
      info@(CotypeConstructorInfo _ _ kind_) = cotypeConstructorInfo loc name def
    DTypeAlias loc name alias -> do
      modify $
        insertAlias name (aliasInfo loc name alias)
          . addName name IAlias
    def@DImport{} -> do
      types <- collect def exportedTypeConstructors
      forM_ types $
        \(TypeConstructorInfo _ name kind_) ->
          modify (addName name (IType kind_))
      cotypes <- collect def exportedCotypeConstructors
      forM_ cotypes $
        \(CotypeConstructorInfo _ name kind_) ->
          modify (addName name (ICotype kind_))
    _ ->
      pure ()

{-# INLINE foldElems #-}
foldElems :: (Monoid m) => (a -> m -> m) -> Environment a -> m
foldElems f = foldr f mempty . Environment.elems

traitEnv :: (Monad m) => StateT ModuleBundle (CompilerT Metadata m) (Environment (TraitInfo Metadata))
traitEnv = do
  gets (foldElems insertTraitInfo . moduleTraits)
 where
  insertTraitInfo :: TraitInfo Metadata -> Environment (TraitInfo Metadata) -> Environment (TraitInfo Metadata)
  insertTraitInfo info@(TraitInfo _ name _ _) = Environment.insert name info

typeConstructorEnv :: (Monad m) => StateT ModuleBundle (CompilerT Metadata m) (Environment Kind)
typeConstructorEnv = do
  env1 <- gets (foldElems insertTypeInfo . moduleTypeConstructors)
  env2 <- gets (foldElems insertCotypeInfo . moduleCotypeConstructors)
  pure (env1 <> env2)
 where
  insertTypeInfo :: TypeConstructorInfo Metadata -> Environment Kind -> Environment Kind
  insertTypeInfo (TypeConstructorInfo _ name kind_) = Environment.insert name kind_

  insertCotypeInfo :: CotypeConstructorInfo Metadata -> Environment Kind -> Environment Kind
  insertCotypeInfo (CotypeConstructorInfo _ name kind_) = Environment.insert name kind_

collectDataConstructors :: (Monad m) => Environment Kind -> Definition Metadata Kind () -> StateT ModuleBundle (CompilerT Metadata m) ()
collectDataConstructors env =
  \case
    DCotype loc _ def ->
      forM_ (codataAccessorInfo env loc def) $
        \info@(CodataAccessorInfo _ _ CodataAccessor{..}) -> do
          modify $
            addName codataAccessorName (ICodataAccessor codataAccessorScheme)
              . insertCodataAccessor codataAccessorName info
    DType loc _ def ->
      forM_ (dataConstructorInfo env loc def) $
        \info@(DataConstructorInfo _ _ DataConstructor{..} names) -> do
          modify $
            addName constructorName (IDataConstructor constructorScheme)
              . insertDataConstructor constructorName info
    def@DImport{} -> do
      ctors <- collect def exportedDataConstructors
      forM_ ctors $
        \(DataConstructorInfo _ _ DataConstructor{..} _) ->
          modify (addName constructorName (IDataConstructor constructorScheme))
    _ ->
      pure ()

collectTraits :: (Monad m) => Environment Kind -> Definition Metadata Kind () -> StateT ModuleBundle (CompilerT Metadata m) ()
collectTraits env =
  \case
    DTrait loc name def -> do
      addTraitEntries env name def
      modify $
        addName name ITrait
          . insertTrait name (traitInfo loc name def)
    _ ->
      pure ()

addTraitEntries :: (Monad m) => Environment Kind -> Name -> TraitDef () -> StateT ModuleBundle (CompilerT Metadata m) ()
addTraitEntries env trait (TraitDef _ p entries) =
  forM_ entries $
    \(name, Forall _ _ t) ->
      modify $
        addName name (IFunction $ scheme [Trait trait tvar] (toIndexedType env p t))
 where
  tvar = TVariable (TypeIndex (parameterKind p) 0)

collectInstances :: (Monad m) => Environment Kind -> Environment (TraitInfo Metadata) -> Definition Metadata Kind () -> StateT ModuleBundle (CompilerT Metadata m) ()
collectInstances kinds traits =
  \case
    DInstance loc trait def ->
      -- (InstanceDef _ q _) -> do
      case Environment.lookup trait traits of
        Nothing ->
          -- TODO
          error "Trait not in scope!"
        Just _ ->
          -- (TraitInfo loc2 _ p TypeIndex{..} dict) ->
          modify $
            -- insertInstance trait undefined (instanceInfo kinds traits trait loc dict)
            undefined -- insertInstance trait undefined (instanceInfo def)
    _ ->
      pure ()
