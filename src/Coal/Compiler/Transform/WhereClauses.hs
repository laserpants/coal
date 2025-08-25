{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Transform.WhereClauses where

import Coal.Compiler.Transform.Tree
import Coal.Language
import Coal.Language.Module
import Control.Monad.Writer
import Data.Data (Data)
import Extra (Name)

liftWhereClause :: (MonadWriter [(Name, Name)] m) => Name -> Definition a k t -> m (Definition a k t)
liftWhereClause name =
  \case
    DFunction old f _ -> do
      -- TODO:  name__$local_old??
      let new = "local_$" <> name <> "__" <> old
      tell [(old, new)]
      pure (DFunction new f [])
    DConstant old c _ -> do
      let new = "local_$" <> name <> "__" <> old
      tell [(old, new)]
      pure (DConstant new c [])
    DAnnotation w d ->
      DAnnotation w <$> liftWhereClause name d
    d ->
      pure d

expandWhereClausesDefinition :: (Data a, Data t, Ord t, MonadWriter [(Name, Name)] m) => Definition a k t -> m [Definition a k t]
expandWhereClausesDefinition =
  \case
    DAnnotation w d -> do
      defs <- expandWhereClausesDefinition d
      case defs of
        [] ->
          error "Implementation error"
        d1 : ds ->
          pure (DAnnotation w d1 : ds)
    DFunction name f ws -> do
      (ds, names) <- listen $ traverse (liftWhereClause name) ws
      pure (replaceNames names <$> (ds <> [DFunction name f []]))
    DConstant name c ws -> do
      (ds, names) <- listen $ traverse (liftWhereClause name) ws
      pure (replaceNames names <$> (ds <> [DConstant name c []]))
    d ->
      pure [d]

replaceNames :: (Data a, Data t, Ord t) => [(Name, Name)] -> Definition a k t -> Definition a k t
replaceNames names =
  \case
    DFunction n f _ ->
      DFunction n (replaceFunctionNames names f) []
    DConstant n c _ ->
      DConstant n (replaceConstantNames names c) []
    DAnnotation w d ->
      DAnnotation w (replaceNames names d)
    d ->
      d

replaceFunctionNames :: (Data a, Data t, Ord t) => [(Name, Name)] -> Function Expression a t -> Function Expression a t
replaceFunctionNames names =
  \case
    Function a w ps e ->
      Function a w ps (foldr (uncurry rename) e names)

replaceConstantNames :: (Data a, Data t, Ord t) => [(Name, Name)] -> Constant Expression a t -> Constant Expression a t
replaceConstantNames names =
  \case
    Constant a w e ->
      Constant a w (foldr (uncurry rename) e names)

expandWhereClausesModule :: (Data a, Data t, Ord t, MonadWriter [(Name, Name)] m) => Module a k t -> m (Module a k t)
expandWhereClausesModule (Module p ns ds) =
  Module p ns . concat <$> traverse expandWhereClausesDefinition ds
