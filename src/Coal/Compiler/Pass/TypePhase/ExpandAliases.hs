{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpandAliases (passExpandAliases) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Environment
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module (ConstantDef (..), Definition (..), FunctionDef (..), InstanceDef (..), Module (..))
import Control.Monad.Reader (asks)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Extras (Dictionary, Name)

passExpandAliases :: (Monad m, Data a) => Pass a m (Module a k ()) (Module a k ())
passExpandAliases =
  Pass
    { passName = "ExpandAliases"
    , runPass = pass
    }

pass :: (Monad m, Data a) => Module a k () -> CompilerT a m (Module a k ())
pass = expandAliases

class TransformContext c where
  expandAliases :: (Monad m) => c -> CompilerT a m c

instance TransformContext () where
  expandAliases _ = pure ()

instance (TransformContext c) => TransformContext [c] where
  expandAliases = traverse expandAliases

instance (TransformContext c) => TransformContext (Maybe c) where
  expandAliases = traverse expandAliases

instance (TransformContext c) => TransformContext (Dictionary c) where
  expandAliases = traverse expandAliases

instance (TransformContext c) => TransformContext (NonEmpty c) where
  expandAliases = traverse expandAliases

instance (TransformContext t) => TransformContext (Trait t) where
  expandAliases = traverse expandAliases

instance (TransformContext t) => TransformContext (With t) where
  expandAliases = traverse expandAliases

instance (TransformContext t) => TransformContext (Row o k t) where
  expandAliases = traverse expandAliases

instance (TransformContext t, Data a, Data t) => TransformContext (Pattern a t) where
  expandAliases =
    transformM $
      \case
        PAnnotation a t p ->
          PAnnotation a <$> expandAliases t <*> expandAliases p
        p ->
          pure p

instance (TransformContext t, Data t, Data a) => TransformContext (Expression a t) where
  expandAliases =
    transformM $
      \case
        EAnnotation a t e ->
          EAnnotation a <$> expandAliases t <*> expandAliases e
        e ->
          pure e

instance (TransformContext t, Data e, Data t) => TransformContext (Module e a t) where
  expandAliases =
    \case
      Module p ns o ->
        Module p ns <$> expandAliases o

instance (TransformContext t, Data a, Data t) => TransformContext (FunctionDef a t) where
  expandAliases =
    \case
      FunctionDef a u w ps e ->
        FunctionDef a
          <$> expandAliases u
          <*> expandAliases w
          <*> expandAliases ps
          <*> expandAliases e

instance (TransformContext t, Data a, Data t) => TransformContext (ConstantDef a t) where
  expandAliases =
    \case
      ConstantDef a u w e ->
        ConstantDef a
          <$> expandAliases u
          <*> expandAliases w
          <*> expandAliases e

instance (TransformContext t, Data a, Data t) => TransformContext (Definition a k t) where
  expandAliases =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> expandAliases f <*> traverse expandAliases fs
      DConstant loc name c fs ->
        DConstant loc name <$> expandAliases c <*> traverse expandAliases fs
      DInstance loc name (InstanceDef ts t ds) ->
        DInstance loc name . InstanceDef ts t <$> traverse expandAliases ds
      o ->
        pure o

instance TransformContext ParameterizedType where
  expandAliases =
    \case
      t@(TApplication _ (TVariable (Parameter _ name)) ts) -> do
        lookupAlias t (toList ts) name
      t@(TApplication _ (TConstructor _ name) ts) -> do
        lookupAlias t (toList ts) name
      TApplication k t ts ->
        TApplication k <$> expandAliases t <*> expandAliases ts
      TArrow t1 t2 ->
        TArrow <$> expandAliases t1 <*> expandAliases t2
      TAlias name ts t ->
        TAlias name <$> expandAliases ts <*> expandAliases t
      TIntrinsic t ->
        pure (TIntrinsic t)
      TRecord t ->
        TRecord <$> expandAliases t
      TRow row ->
        TRow <$> traverse expandAliases row
      t@(TVariable (Parameter _ name)) ->
        lookupAlias t [] name
      t@(TConstructor _ name) ->
        lookupAlias t [] name

lookupAlias :: (Monad m) => ParameterizedType -> [ParameterizedType] -> Name -> CompilerT a m ParameterizedType
lookupAlias t ts name = do
  env <- asks compilerAliasEnvironment
  case Environment.lookup name env of
    Nothing ->
      pure t
    Just AliasEntry{..} ->
      pure (TAlias name ts (foldr (uncurry substituteAlias) aliasEntryType (aliasEntryParams `zip` ts)))

substituteAlias :: Name -> Type Parameter k -> Type Parameter k -> Type Parameter k
substituteAlias name s =
  \case
    t@(TVariable (Parameter _ match))
      | name == match ->
          s
      | otherwise ->
          t
    TApplication k t1 ts ->
      TApplication k (substituteAlias name s t1) (substituteAlias name s <$> ts)
    TArrow t1 t2 ->
      TArrow (substituteAlias name s t1) (substituteAlias name s t2)
    TRow row ->
      TRow (substituteAlias name s <$> row)
    t ->
      t
