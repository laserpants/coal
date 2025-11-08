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
import Control.Monad.State (StateT, execStateT, gets, lift, modify)
import Data.List ((\\))
import Extras (Name, forM_)

build :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m ModuleBundle
build (Module _ exports defs) =
  flip execStateT emptyModuleBundle $ do
    modify (setExports exports)
    inEachDef collectTypeConstructors
    kinds <- typeConstructorEnv
    inEachDef (collectDataConstructors kinds)
    inEachDef (collectTraits kinds)
    traits <- traitEnv
    inEachDef (collectInstances kinds traits)
 where
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

traitEnv :: (Monad m) => StateT ModuleBundle (CompilerT Metadata m) (Environment TraitInfo)
traitEnv =
  undefined

typeConstructorEnv :: (Monad m) => StateT ModuleBundle (CompilerT Metadata m) (Environment Kind)
typeConstructorEnv = do
  env1 <- gets (collect_ insertTypeInfo . moduleTypeConstructors)
  env2 <- gets (collect_ insertCotypeInfo . moduleCotypeConstructors)
  pure (env1 <> env2)
 where
  collect_ f = foldr f mempty . Environment.elems

  insertTypeInfo :: TypeConstructorInfo -> Environment Kind -> Environment Kind
  insertTypeInfo (TypeConstructorInfo _ name kind_) = Environment.insert name kind_

  insertCotypeInfo :: CotypeConstructorInfo -> Environment Kind -> Environment Kind
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
    DTrait loc name def ->
      modify $
        addName name ITrait
          . insertTrait name (traitInfo env loc name def)
    _ ->
      pure ()

collectInstances :: (Monad m) => Environment Kind -> Environment TraitInfo -> Definition Metadata Kind () -> StateT ModuleBundle (CompilerT Metadata m) ()
collectInstances kinds traits =
  \case
    DInstance _ name def ->
      case Environment.lookup name traits of
        Nothing ->
          -- TODO
          error "Trait not in scope!"
        Just (TraitInfo loc _ p TypeIndex{..} dict) ->
          undefined
    _ ->
      pure ()
