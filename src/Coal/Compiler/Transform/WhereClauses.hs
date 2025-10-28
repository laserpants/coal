{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Transform.WhereClauses (expandWhereClausesModule) where

import Coal.Compiler.Journal
import Coal.Compiler.Stack
import Coal.Compiler.Transform.Tree
import Coal.Language.Module
import Data.Data (Data)
import Extras (Name)

liftWhereClause :: (Monad m) => Name -> Definition a k t -> CompilerT a m (Definition a k t)
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

manufacturedName :: (Monad m) => Name -> Name -> CompilerT a m Name
manufacturedName name old = do
  tellWhereClauses [(old, new)]
  pure new
 where
  new = name <> "__$local_" <> old

expandWhereClausesModule :: (Monad m, Data a, Ord t, Data t) => Module a k t -> CompilerT a m (Module a k t)
expandWhereClausesModule (Module p ns ds) =
  Module p ns . concat <$> traverse expandWhereClausesDefinition ds

expandWhereClausesDefinition :: (Monad m, Data a, Ord t, Data t) => Definition a k t -> CompilerT a m [Definition a k t]
expandWhereClausesDefinition =
  \case
    DFunction loc name f ws -> do
      (ds, names) <- listenWhereClauses $ traverse (liftWhereClause name) ws
      pure (replaceNames names <$> (ds <> [DFunction loc name f []]))
    DConstant loc name c ws -> do
      (ds, names) <- listenWhereClauses $ traverse (liftWhereClause name) ws
      pure (replaceNames names <$> (ds <> [DConstant loc name c []]))
    d ->
      pure [d]

replaceNames :: (Data a, Data t, Ord t) => [(Name, Name)] -> Definition a k t -> Definition a k t
replaceNames names =
  \case
    DFunction loc n f _ ->
      DFunction loc n (fmap (replaceFunctionNames names) f) []
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
