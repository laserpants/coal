{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpandAliases (passExpandAliases) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Environment
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module (ConstantDefinition (..), Definition (..), FunctionDefinition (..), InstanceDefinition (..), Module (..))
import Control.Monad.Reader (asks)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Extras (Dictionary, Name)

passExpandAliases :: (Monad m, Data a) => Pass a m (Module a k ()) (Module a k ())
passExpandAliases = Pass{runPass = pass}

pass :: (Monad m, Data a) => Module a k () -> CompilerT a m (Module a k ())
pass = expandAliases

class AliasContext c where
  expandAliases :: (Monad m) => c -> CompilerT a m c

instance AliasContext () where
  expandAliases _ = pure ()

instance (AliasContext c) => AliasContext [c] where
  expandAliases = traverse expandAliases

instance (AliasContext c) => AliasContext (Maybe c) where
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

instance (AliasContext t, Data a, Data t) => AliasContext (FunctionDefinition a t) where
  expandAliases =
    \case
      FunctionDefinition a u w ps e ->
        FunctionDefinition a
          <$> expandAliases u
          <*> expandAliases w
          <*> expandAliases ps
          <*> expandAliases e

instance (AliasContext t, Data a, Data t) => AliasContext (ConstantDefinition a t) where
  expandAliases =
    \case
      ConstantDefinition a u w e ->
        ConstantDefinition a
          <$> expandAliases u
          <*> expandAliases w
          <*> expandAliases e

instance (AliasContext t, Data a, Data t) => AliasContext (Definition a k t) where
  expandAliases =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> expandAliases f <*> traverse expandAliases fs
      DConstant loc name c fs ->
        DConstant loc name <$> expandAliases c <*> traverse expandAliases fs
      DInstance loc name (InstanceDefinition ts t ds) ->
        DInstance loc name . InstanceDefinition ts t <$> traverse expandAliases ds
      o ->
        pure o

expandAliasesTypeApplication :: (Monad m) => ParameterizedType -> ParameterizedType -> NonEmpty ParameterizedType -> CompilerT a m ParameterizedType
expandAliasesTypeApplication t (TVariable (Parameter _ name)) ts =
  lookupAlias t (toList ts) name
expandAliasesTypeApplication t (TConstructor _ name) ts =
  lookupAlias t (toList ts) name
expandAliasesTypeApplication _ t ts =
  applyTypeArgs () <$> expandAliases t <*> expandAliases ts

instance AliasContext ParameterizedType where
  expandAliases =
    \case
      t@TApplication{} ->
        uncurry (expandAliasesTypeApplication t) (listTypeArgs t)
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
    TApplication k t1 t2 ->
      TApplication k (substituteAlias name s t1) (substituteAlias name s t2)
    TArrow t1 t2 ->
      TArrow (substituteAlias name s t1) (substituteAlias name s t2)
    TRow row ->
      TRow (substituteAlias name s <$> row)
    t ->
      t
