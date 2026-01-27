{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Coal.ProtoTypeSystem.Parameterized (
  ProtoParameterized (..),
  ToIndexed (..),
) where

import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (MonadState)
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name)
import Extras.Control.Monad (concatMapM)
import Extras.Operators ((<>^))

class ToIndexed a b where
  toIndexed :: (MonadState s m, Supply s) => a -> ReaderT (Environment (TypeIndex Kind)) m b

instance (ToIndexed a b) => ToIndexed [a] [b] where
  toIndexed = traverse toIndexed

instance ToIndexed (Trait (Type Parameter Kind)) (Trait (Type TypeIndex Kind)) where
  toIndexed =
    \case
      Trait name t ->
        Trait name <$> toIndexed t

instance (Ord b, ToIndexed a b) => ToIndexed (Set a) (Set b) where
  toIndexed s = Set.fromList <$> toIndexed (Set.toList s)

instance ToIndexed (Type Parameter Kind) (Type TypeIndex Kind) where
  toIndexed =
    \case
      TVariable p ->
        TVariable <$> toIndexed p
      TApplication k t1 t2 ->
        TApplication k <$> toIndexed t1 <*> toIndexed t2
      TArrow t1 t2 -> do
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

class ProtoParameterized p where
  protoOinstantiateTypeIndexes :: (MonadState s m, Supply s) => p -> m [(Name, TypeIndex Kind)]

instance (ProtoParameterized p) => ProtoParameterized [p] where
  protoOinstantiateTypeIndexes = concatMapM protoOinstantiateTypeIndexes

instance (ProtoParameterized p) => ProtoParameterized (Set p) where
  protoOinstantiateTypeIndexes = protoOinstantiateTypeIndexes . Set.toList

instance ProtoParameterized (Type Parameter Kind) where
  protoOinstantiateTypeIndexes =
    \case
      TVariable p ->
        protoOinstantiateTypeIndexes p
      TApplication _ t1 t2 ->
        protoOinstantiateTypeIndexes t1 <>^ protoOinstantiateTypeIndexes t2
      TArrow t1 t2 -> do
        protoOinstantiateTypeIndexes t1 <>^ protoOinstantiateTypeIndexes t2
      TRecord t ->
        protoOinstantiateTypeIndexes t
      TRow r ->
        protoOinstantiateTypeIndexes r
      TAlias _ _ t ->
        protoOinstantiateTypeIndexes t
      TConstructor{} ->
        pure []
      TIntrinsic{} ->
        pure []

instance ProtoParameterized (Row Parameter Kind (Type Parameter Kind)) where
  protoOinstantiateTypeIndexes =
    \case
      RVariable p ->
        protoOinstantiateTypeIndexes p
      RExtend _ t r -> do
        protoOinstantiateTypeIndexes t <>^ protoOinstantiateTypeIndexes r
      RNil ->
        pure []

instance ProtoParameterized (Parameter Kind) where
  protoOinstantiateTypeIndexes (Parameter kind name) = do
    index <- supplied (TypeIndex kind)
    pure [(name, index)]
