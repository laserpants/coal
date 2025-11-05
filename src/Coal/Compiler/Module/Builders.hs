{-# LANGUAGE LambdaCase #-}

module Coal.Compiler.Module.Builders where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Module.Bundle
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad.State (StateT, execStateT, gets, lift, modify)
import Extras (Name, forM_)

build :: (Monad m) => Module Metadata Kind () -> CompilerT Metadata m ModuleBundle
build (Module _ exports defs) =
  flip execStateT emptyModuleBundle $ do
    modify (setExports exports)
    forM_ defs collectTypeConstructors
    undefined

pick :: [Name] -> Environment a -> [a]
pick names = Environment.elems . Environment.restrict names

collectTypeConstructors :: (Monad m) => Definition Metadata Kind () -> StateT ModuleBundle (CompilerT Metadata m) ()
collectTypeConstructors =
  \case
    DType loc name def -> do
      let info@(TypeConstructorInfo _ _ kind) = typeConstructorInfo loc name def
      modify $
        insertTypeConstructor name info . addName name (IType kind)
    DCotype loc name def -> do
      let info@(CotypeConstructorInfo _ _ kind) = cotypeConstructorInfo loc name def
      modify $
        insertCotypeConstructor name info . addName name (ICotype kind)
    DTypeAlias loc name alias -> do
      modify $
        insertAlias name (aliasInfo loc name alias)
          . addName name IAlias
    DImport loc path names -> do
      env <- lift (gets compilerModules)
      case Environment.lookup (principalPath path) env of
        Nothing ->
          -- TODO: No such module
          undefined
        Just bundle -> do
          -- TODO: check for missing names
          forM_ (pick names (exportedTypeConstructors bundle)) $
            \(TypeConstructorInfo _ name kind) ->
              modify (addName name (IType kind))
          forM_ (pick names (exportedCotypeConstructors bundle)) $
            \(CotypeConstructorInfo _ name kind) ->
              modify (addName name (ICotype kind))
    _ ->
      pure ()
