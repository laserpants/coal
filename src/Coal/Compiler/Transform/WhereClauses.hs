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
    DFunction loc old with f _ -> do
      new <- fabricatedName name old
      pure (DFunction loc new with f [])
    DConstant loc old with c _ -> do
      new <- fabricatedName name old
      pure (DConstant loc new with c [])
    d ->
      pure d

fabricatedName :: (MonadWriter [(Name, Name)] m) => Name -> Name -> m Name
fabricatedName name old = do
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
    DFunction loc name with f ws -> do
      (ds, names) <- listen $ traverse (liftWhereClause name) ws
      pure (replaceNames names <$> (ds <> [DFunction loc name with f []]))
    DConstant loc name with c ws -> do
      (ds, names) <- listen $ traverse (liftWhereClause name) ws
      pure (replaceNames names <$> (ds <> [DConstant loc name with c []]))
    d ->
      pure [d]

replaceNames :: (Data a, Data t, Ord t) => [(Name, Name)] -> Definition a k t -> Definition a k t
replaceNames names =
  \case
    DFunction loc n with f _ ->
      DFunction loc n with (replaceFunctionNames names f) []
    DConstant loc n with c _ ->
      DConstant loc n with (replaceConstantNames names c) []
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
