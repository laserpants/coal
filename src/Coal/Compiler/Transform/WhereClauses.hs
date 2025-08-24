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
      let new = "local_$" <> name <> "__" <> old
      pure (DFunction new f [])
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
    d ->
      pure [d]

replaceNames :: (Data a, Data t, Ord t) => [(Name, Name)] -> Definition a k t -> Definition a k t
replaceNames names =
  \case
    DFunction n f _ ->
      DFunction n (replaceFunctionNames names f) []
    DAnnotation w d ->
      DAnnotation w (replaceNames names d)
    d ->
      d

replaceFunctionNames :: (Data a, Data t, Ord t) => [(Name, Name)] -> Function Expression a t -> Function Expression a t
replaceFunctionNames names =
  \case
    Function a w ps e ->
      Function a w ps (foldr (uncurry rename) e names)

expandWhereClausesModule :: (Data a, Data t, Ord t, MonadWriter [(Name, Name)] m) => Module a k t -> m (Module a k t)
expandWhereClausesModule (Module p ns ds) =
  Module p ns . concat <$> traverse expandWhereClausesDefinition ds
