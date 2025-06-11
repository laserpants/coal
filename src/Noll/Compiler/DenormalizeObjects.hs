{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Compiler.DenormalizeObjects (DenormalizeObjectsTransformContext (..)) where

import Data.Data (Data, Typeable)
import Data.Map.Strict (Map)
import Lang.Common.List1 (List1)
import Lang.Utils (Name)
import Noll.Compiler.Transform
import Noll.Language.Expression (Expression (..))
import Noll.Language.HasType (HasType (..), foldTypeOf)
import Noll.Language.Trait (With (..))
import Noll.Language.Type (Type (..))
import Noll.Module (Module (..))
import Noll.Module.Constant (Constant (..))
import Noll.Module.Definition (Definition (..))
import Noll.Module.Function (Function (..))

class DenormalizeObjectsTransformContext a where
  denormalizeObject :: a -> a

instance (DenormalizeObjectsTransformContext a) => DenormalizeObjectsTransformContext [a] where
  denormalizeObject = fmap denormalizeObject

instance (DenormalizeObjectsTransformContext a) => DenormalizeObjectsTransformContext (List1 a) where
  denormalizeObject = fmap denormalizeObject

instance (DenormalizeObjectsTransformContext a) => DenormalizeObjectsTransformContext (Map k a) where
  denormalizeObject = fmap denormalizeObject

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => DenormalizeObjectsTransformContext (Module a k (Type o k)) where
  denormalizeObject =
    \case
      Module p ns d ->
        Module p ns (denormalizeObject d)

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => DenormalizeObjectsTransformContext (Definition a k (Type o k)) where
  denormalizeObject =
    \case
      DAnnotation u d ->
        DAnnotation u (denormalizeObject d)
      DConstant name c ->
        denormalizeConstant name c
      DInstance name t ds ->
        DInstance name t (denormalizeObject ds)
      DInstance2 name t ds ->
        DInstance2 name t (denormalizeObject ds)
      d ->
        d

denormalizeConstant :: (Data a, Data k, Data (o k), Typeable o) => Name -> Constant Expression a (Type o k) -> Definition a k (Type o k)
denormalizeConstant name =
  \case
    Constant a with (ELambda a1 ps (ELambda _ qs e)) ->
      denormalizeConstant name (Constant a with (ELambda a1 (ps <> qs) e))
    Constant a (With ts t) (ELambda _ ps e) ->
      DFunction name (Function a (With ts (typeOf e)) ps e)
    c@Constant{} ->
      DConstant name c
