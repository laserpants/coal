{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Transform.WhereClauses (expandWhereClausesModule) where

import Coal.Compiler.Transform.Tree
import Coal.Language
import Coal.Language.Module
import Control.Monad.Writer
import Data.Data (Data)
import Extra (Name)

liftWhereClause :: (MonadWriter [(Name, Name)] m) => Name -> Definition a k t -> m (Definition a k t)
liftWhereClause name =
  \case
    DFunction loc old f _ -> do
      new <- manufacturedName name old
      pure (DFunction loc new f [])
    DConstant loc old c _ -> do
      new <- manufacturedName name old
      pure (DConstant loc new c [])
    d ->
      pure d

manufacturedName :: (MonadWriter [(Name, Name)] m) => Name -> Name -> m Name
manufacturedName name old = do
  tell [(old, new)]
  pure new
 where
  new = name <> "__$local_" <> old

expandWhereClausesModule :: (Data a, Data t, Ord t, MonadWriter [(Name, Name)] m) => Module a k t -> m (Module a k t)
expandWhereClausesModule (Module p ns ds) =
  Module p ns . concat <$> traverse expandWhereClausesDefinition ds

expandWhereClausesDefinition :: (Data a, Data t, Ord t, MonadWriter [(Name, Name)] m) => Definition a k t -> m [Definition a k t]
expandWhereClausesDefinition =
  \case
    --    DAnnotation loc w d -> do
    --      defs <- expandWhereClausesDefinition d
    --      case defs of
    --        [] ->
    --          error "Implementation error"
    --        d1 : ds ->
    --          pure (DAnnotation loc w d1 : ds)
    DFunction loc name f ws -> do
      (ds, names) <- listen $ traverse (liftWhereClause name) ws
      pure (replaceNames names <$> (ds <> [DFunction loc name f []]))
    DConstant loc name c ws -> do
      (ds, names) <- listen $ traverse (liftWhereClause name) ws
      pure (replaceNames names <$> (ds <> [DConstant loc name c []]))
    d ->
      pure [d]

replaceNames :: (Data a, Data t, Ord t) => [(Name, Name)] -> Definition a k t -> Definition a k t
replaceNames names =
  \case
    DFunction loc n f _ ->
      DFunction loc n (replaceFunctionNames names f) []
    DConstant loc n c _ ->
      DConstant loc n (replaceConstantNames names c) []
    d ->
      d

replaceFunctionNames :: (Data a, Data t, Ord t) => [(Name, Name)] -> FunctionDef a t -> FunctionDef a t
replaceFunctionNames names =
  \case
    FunctionDef a u w ps e ->
      FunctionDef a u w ps (foldr (uncurry rename) e names)

replaceConstantNames :: (Data a, Data t, Ord t) => [(Name, Name)] -> ConstantDef a t -> ConstantDef a t
replaceConstantNames names =
  \case
    ConstantDef a u w e ->
      ConstantDef a u w (foldr (uncurry rename) e names)
