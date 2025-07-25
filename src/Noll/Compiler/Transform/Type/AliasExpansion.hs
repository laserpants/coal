{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Type.AliasExpansion (
  AliasEnvironment,
  AliasContext (..),
) where

import Control.Monad.Reader (MonadReader, ask)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Noll.Common.Environment (Environment)
import Noll.Common.List1 (NonEmpty (..), fromList1)
import Extra (Dictionary, Name)
import Noll.Language
import Noll.Language.Module (Constant (..), Definition (..), Function (..), Module (..))

import qualified Noll.Common.Environment as Environment

type ParameterizedType = Type Parameter ()

type AliasEnvironment = Environment ([Name], ParameterizedType)

class AliasContext c where
  expandAliases :: (MonadReader AliasEnvironment m) => c -> m c

instance AliasContext () where
  expandAliases _ = pure ()

instance (AliasContext c) => AliasContext [c] where
  expandAliases = traverse expandAliases

instance (AliasContext c) => AliasContext (Dictionary c) where
  expandAliases = traverse expandAliases

instance (AliasContext c) => AliasContext (NonEmpty c) where
  expandAliases = traverse expandAliases

instance (AliasContext t) => AliasContext (Trait t) where
  expandAliases = traverse expandAliases

instance (AliasContext t) => AliasContext (With t) where
  expandAliases = traverse expandAliases

instance (AliasContext t) => AliasContext (Row o k t) where
  expandAliases = traverse expandAliases

instance (AliasContext t, Data a, Data t) => AliasContext (Pattern a t) where
  expandAliases =
    transformM $
      \case
        PAnnotation a t p ->
          PAnnotation a <$> expandAliases t <*> expandAliases p
        p ->
          pure p

instance (AliasContext t, Data t, Data a) => AliasContext (Expression a t) where
  expandAliases =
    transformM $
      \case
        EAnnotation a t e ->
          EAnnotation a <$> expandAliases t <*> expandAliases e
        e ->
          pure e

instance (AliasContext t, Data e, Data t) => AliasContext (Module e a t) where
  expandAliases =
    \case
      Module p ns o ->
        Module p ns <$> expandAliases o

instance (AliasContext (e a t), AliasContext t, Data a, Data t) => AliasContext (Function e a t) where
  expandAliases =
    \case
      Function a u ps e ->
        Function a
          <$> expandAliases u
          <*> expandAliases ps
          <*> expandAliases e

instance (AliasContext (e a t), AliasContext t) => AliasContext (Constant e a t) where
  expandAliases =
    \case
      Constant a u e ->
        Constant a
          <$> expandAliases u
          <*> expandAliases e

instance (AliasContext t, Data a, Data t) => AliasContext (Definition a k t) where
  expandAliases =
    \case
      DAnnotation u o ->
        DAnnotation <$> expandAliases u <*> expandAliases o
      DFunction name f ->
        DFunction name <$> expandAliases f
      DConstant name c ->
        DConstant name <$> expandAliases c
      DInstance name t ds ->
        DInstance name t <$> traverse expandAliases ds
      o ->
        pure o

instance AliasContext ParameterizedType where
  expandAliases =
    \case
      t@(TApplication _ (TVariable (Parameter _ name)) ts) -> do
        lookupAlias t (fromList1 ts) name
      t@(TApplication _ (TConstructor _ name) ts) -> do
        lookupAlias t (fromList1 ts) name
      TApplication k t ts ->
        TApplication k <$> expandAliases t <*> expandAliases ts
      TArrow t1 t2 ->
        TArrow <$> expandAliases t1 <*> expandAliases t2
      TAlias name ts t ->
        TAlias name <$> expandAliases ts <*> expandAliases t
      TIntrinsic t ->
        TIntrinsic <$> traverse expandAliases t
      TRow row ->
        TRow <$> traverse expandAliases row
      t@(TVariable (Parameter _ name)) ->
        lookupAlias t [] name
      t@(TConstructor _ name) ->
        lookupAlias t [] name

lookupAlias :: (MonadReader AliasEnvironment m) => ParameterizedType -> [ParameterizedType] -> Name -> m ParameterizedType
lookupAlias t ts name = do
  env <- ask
  case Environment.lookup name env of
    Nothing ->
      pure t
    Just (ns, t1) ->
      pure (TAlias name ts (foldr (uncurry substituteAlias) t1 (ns `zip` ts)))

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
