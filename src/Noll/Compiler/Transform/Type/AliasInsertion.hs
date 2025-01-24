{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Type.AliasInsertion (
  AliasEnvironment,
  AliasContext (..),
) where

import Control.Monad.Reader (MonadReader, ask)
import Noll.Common.Environment (Environment)
import Noll.Common.List1 (NonEmpty (..), fromList1)
import Noll.Compiler.Transform.Expression (mapMOverExpression)
import Noll.Compiler.Transform.Pattern (mapMOverPattern)
import Noll.Language (
  Constant (..),
  Expression (..),
  Function (..),
  IndexedType,
  Module (..),
  Object (..),
  Parameter (..),
  Pattern (..),
  Row (..),
  Trait (..),
  Type (..),
  Uses (..),
 )
import Noll.Utils (Dictionary, Name)

import qualified Noll.Common.Environment as Environment

type ParameterizedType = Type Parameter ()

type AliasEnvironment = Environment ([Name], ParameterizedType)

class AliasContext c where
  insertAliases :: (MonadReader AliasEnvironment m) => c -> m c

instance AliasContext () where
  insertAliases _ = pure ()

instance (AliasContext c) => AliasContext [c] where
  insertAliases = traverse insertAliases

instance (AliasContext c) => AliasContext (Dictionary c) where
  insertAliases = traverse insertAliases

instance (AliasContext c) => AliasContext (NonEmpty c) where
  insertAliases = traverse insertAliases

instance (AliasContext t) => AliasContext (Trait t) where
  insertAliases = traverse insertAliases

instance (AliasContext t) => AliasContext (Uses t) where
  insertAliases = traverse insertAliases

instance (AliasContext t) => AliasContext (Row o k t) where
  insertAliases = traverse insertAliases

instance (AliasContext t) => AliasContext (Pattern a t) where
  insertAliases =
    mapMOverPattern $
      \case
        PAnnotation a t p ->
          PAnnotation a <$> insertAliases t <*> insertAliases p
        p ->
          pure p

instance (AliasContext t) => AliasContext (Expression a t) where
  insertAliases =
    mapMOverExpression $
      \case
        EAnnotation a t e ->
          EAnnotation a <$> insertAliases t <*> insertAliases e
        e ->
          pure e

instance (AliasContext t) => AliasContext (Module e a t) where
  insertAliases = undefined

instance (AliasContext (e a t), AliasContext t) => AliasContext (Function e a t) where
  insertAliases = undefined

instance (AliasContext (e a t), AliasContext t) => AliasContext (Constant e a t) where
  insertAliases = undefined

instance (AliasContext t) => AliasContext (Object a k t) where
  insertAliases = undefined

instance AliasContext (ParameterizedType) where
  insertAliases =
    \case
      t@(TApplication _ (TVariable (Parameter _ name)) ts) -> do
        lookupAlias t (fromList1 ts) name
      t@(TApplication _ (TConstructor _ name) ts) -> do
        lookupAlias t (fromList1 ts) name
      TApplication k t ts ->
        TApplication k <$> insertAliases t <*> insertAliases ts
      TArrow t1 t2 ->
        TArrow <$> insertAliases t1 <*> insertAliases t2
      TAlias name ts t ->
        TAlias name <$> insertAliases ts <*> insertAliases t
      TIntrinsic t ->
        TIntrinsic <$> traverse insertAliases t
      TRow row ->
        TRow <$> traverse insertAliases row
      t@(TVariable (Parameter _ name)) ->
        lookupAlias t [] name
      t@(TConstructor _ name) ->
        lookupAlias t [] name

lookupAlias :: (MonadReader AliasEnvironment m) => ParameterizedType -> [ParameterizedType] -> Name -> m (ParameterizedType)
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
