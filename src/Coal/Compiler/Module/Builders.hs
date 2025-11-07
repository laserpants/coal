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
    forM_ defs collectTypeConstructors
    forM_ defs collectDataConstructors

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
        insertTypeConstructor name info . addName name (IType kind)
     where
      info@(TypeConstructorInfo _ _ kind) = typeConstructorInfo loc name def
    DCotype loc name def -> do
      modify $
        insertCotypeConstructor name info . addName name (ICotype kind)
     where
      info@(CotypeConstructorInfo _ _ kind) = cotypeConstructorInfo loc name def
    DTypeAlias loc name alias -> do
      modify $
        insertAlias name (aliasInfo loc name alias)
          . addName name IAlias
    def@DImport{} -> do
      types <- collect def exportedTypeConstructors
      forM_ types $
        \(TypeConstructorInfo _ name kind) ->
          modify (addName name (IType kind))
      cotypes <- collect def exportedCotypeConstructors
      forM_ cotypes $
        \(CotypeConstructorInfo _ name kind) ->
          modify (addName name (ICotype kind))
    _ ->
      pure ()

collectDataConstructors :: (Monad m) => Definition Metadata Kind () -> StateT ModuleBundle (CompilerT Metadata m) ()
collectDataConstructors =
  \case
    DCotype loc name def -> do
      forM_ (codataAccessorInfo loc def) $
        \(CodataAccessorInfo _ _ CodataAccessor{..}) -> do
          modify (addName codataAccessorName (ICodataAccessor codataAccessorScheme))
    DType loc _ def -> do
      forM_ (dataConstructorInfo loc def) $
        \(DataConstructorInfo _ _ DataConstructor{..} names) -> do
          modify (addName constructorName (IDataConstructor constructorScheme))
    def@DImport{} -> do
      ctors <- collect def exportedDataConstructors
      forM_ ctors $
        \(DataConstructorInfo _ _ DataConstructor{..} _) ->
          modify (addName constructorName (IDataConstructor constructorScheme))
    _ ->
      pure ()

collectTraits =
  undefined

collectInstances =
  \case
    _ ->
      undefined
