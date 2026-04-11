{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.TypeSystem.Parameterized (
  Parameterized (..),
  ToIndexed (..),
  replaceParamInScheme,
) where

import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (MonadState)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name)
import Extras.Control.Monad (concatMapM)
import Extras.Data.Set (unionMap)
import Extras.Operators ((<>^))

class ToIndexed i o where
  toIndexed :: (MonadState s m, Supply s) => i -> ReaderT (Environment (TypeIndex Kind)) m o

instance (ToIndexed i o) => ToIndexed [i] [o] where
  toIndexed = traverse toIndexed

instance ToIndexed (Trait (Type Parameter Kind)) (Trait (Type TypeIndex Kind)) where
  toIndexed =
    \case
      Trait name t ->
        Trait name <$> toIndexed t

instance (Ord o, ToIndexed i o) => ToIndexed (Set i) (Set o) where
  toIndexed s = Set.fromList <$> toIndexed (Set.toList s)

instance ToIndexed (Type Parameter Kind) (Type TypeIndex Kind) where
  toIndexed =
    \case
      TVariable p ->
        TVariable <$> toIndexed p
      TApplication k t1 t2 ->
        TApplication k <$> toIndexed t1 <*> toIndexed t2
      TArrow t1 t2 ->
        TArrow <$> toIndexed t1 <*> toIndexed t2
      TRecord t ->
        TRecord <$> toIndexed t
      TRow r ->
        TRow <$> toIndexed r
      TAlias name ts t ->
        TAlias name <$> traverse toIndexed ts <*> toIndexed t
      TConstructor k name ->
        pure (TConstructor k name)
      TIntrinsic t ->
        pure (TIntrinsic t)

instance ToIndexed (Row Parameter Kind (Type Parameter Kind)) (Row TypeIndex Kind (Type TypeIndex Kind)) where
  toIndexed =
    \case
      RExtend name t r ->
        RExtend name <$> toIndexed t <*> toIndexed r
      RVariable p ->
        RVariable <$> toIndexed p
      RNil ->
        pure RNil

instance ToIndexed (Parameter Kind) (TypeIndex Kind) where
  toIndexed =
    \case
      Parameter kind name -> do
        env <- ask
        case Environment.lookup name env of
          Nothing ->
            supplied (TypeIndex kind)
          Just index ->
            pure index

class Parameterized p where
  instantiateTypeIndexes :: (MonadState s m, Supply s) => p -> m [(Name, TypeIndex Kind)]

instance (Parameterized p) => Parameterized [p] where
  instantiateTypeIndexes = concatMapM instantiateTypeIndexes

instance (Parameterized p) => Parameterized (Set p) where
  instantiateTypeIndexes = instantiateTypeIndexes . Set.toList

instance Parameterized (Type Parameter Kind) where
  instantiateTypeIndexes =
    \case
      TVariable p ->
        instantiateTypeIndexes p
      TApplication _ t1 t2 ->
        instantiateTypeIndexes t1 <>^ instantiateTypeIndexes t2
      TArrow t1 t2 ->
        instantiateTypeIndexes t1 <>^ instantiateTypeIndexes t2
      TRecord t ->
        instantiateTypeIndexes t
      TRow r ->
        instantiateTypeIndexes r
      TAlias _ _ t ->
        instantiateTypeIndexes t
      TConstructor{} ->
        pure []
      TIntrinsic{} ->
        pure []

instance Parameterized (Row Parameter Kind (Type Parameter Kind)) where
  instantiateTypeIndexes =
    \case
      RVariable p ->
        instantiateTypeIndexes p
      RExtend _ t r ->
        instantiateTypeIndexes t <>^ instantiateTypeIndexes r
      RNil ->
        pure []

instance Parameterized (Parameter Kind) where
  instantiateTypeIndexes (Parameter kind name) = do
    index <- supplied (TypeIndex kind)
    pure [(name, index)]

replaceParamInScheme :: Parameter Kind -> Type Parameter Kind -> Scheme Parameter Kind (Type Parameter Kind) -> Scheme Parameter Kind (Type Parameter Kind)
replaceParamInScheme p o Forall{..} =
  Forall
    (paramsIn o <> Set.filter (on (/=) parameterName p) schemeTypeVariables)
    (Set.map replaceParamTrait schemeTraits)
    (replaceParam schemeTypeBody)
 where
  paramsIn =
    \case
      TApplication _ t1 t2 ->
        paramsIn t1 <> paramsIn t2
      TArrow t1 t2 ->
        paramsIn t1 <> paramsIn t2
      TRecord t ->
        paramsIn t
      TRow r ->
        paramsInRow r
      TVariable p ->
        Set.singleton p
      TAlias _ ts t ->
        unionMap paramsIn ts <> paramsIn t
      _ ->
        mempty
  paramsInRow =
    \case
      RExtend _ t r ->
        paramsIn t <> paramsInRow r
      RVariable p ->
        Set.singleton p
      _ ->
        mempty
  replaceParamTrait =
    \case
      Trait name t ->
        Trait name (replaceParam t)
  replaceParam =
    \case
      TApplication k t1 t2 ->
        TApplication k (replaceParam t1) (replaceParam t2)
      TArrow t1 t2 ->
        TArrow (replaceParam t1) (replaceParam t2)
      TRecord t ->
        TRecord (replaceParam t)
      TRow r ->
        TRow (replaceParamRow r)
      TVariable q
        | parameterName p == parameterName q -> o
        | otherwise -> TVariable q
      TAlias name ts t ->
        TAlias name (fmap replaceParam ts) (replaceParam t)
      t ->
        t
  replaceParamRow =
    \case
      RExtend name t r ->
        RExtend name (replaceParam t) (replaceParamRow r)
      r ->
        r
