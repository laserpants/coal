{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Compiler.Transform.NormalizeObjects (NormalizeObjectsTransformContext (..)) where

import Data.Data (Data, Typeable)
import Data.Map.Strict (Map)
import Extra (Name)
import Coal.Common.List1 (List1)
import Coal.Compiler.Transform.Flattening
import Coal.Language.Expression (Expression (..))
import Coal.Language.HasType (HasType (..), foldTypeOf)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Constant (Constant (..))
import Coal.Language.Module.Definition (Definition (..))
import Coal.Language.Module.Function (Function (..))
import Coal.Language.Trait (With (..))
import Coal.Language.Type (Type (..))

class NormalizeObjectsTransformContext a where
  normalizeObject :: a -> a
  denormalizeObject :: a -> a

instance (NormalizeObjectsTransformContext a) => NormalizeObjectsTransformContext [a] where
  normalizeObject = fmap normalizeObject
  denormalizeObject = fmap denormalizeObject

instance (NormalizeObjectsTransformContext a) => NormalizeObjectsTransformContext (List1 a) where
  normalizeObject = fmap normalizeObject
  denormalizeObject = fmap denormalizeObject

instance (NormalizeObjectsTransformContext a) => NormalizeObjectsTransformContext (Map k a) where
  normalizeObject = fmap normalizeObject
  denormalizeObject = fmap denormalizeObject

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => NormalizeObjectsTransformContext (Module a k (Type o k)) where
  normalizeObject =
    \case
      Module p ns d ->
        Module p ns (normalizeObject d)
  denormalizeObject =
    \case
      Module p ns d ->
        Module p ns (denormalizeObject d)

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => NormalizeObjectsTransformContext (Definition a k (Type o k)) where
  normalizeObject =
    \case
      DAnnotation u d ->
        DAnnotation u (normalizeObject d)
      DFunction name (Function a (With ts t) ps e) ->
        DConstant name (Constant a (With ts (foldTypeOf t ps)) (flattenLambda (ELambda mempty ps e)))
      DInstance name t ds ->
        DInstance name t (normalizeObject ds)
      d ->
        d
  denormalizeObject =
    \case
      DAnnotation u d ->
        DAnnotation u (denormalizeObject d)
      DConstant name c ->
        denormalizeConstant name c
      DInstance name t ds ->
        DInstance name t (denormalizeObject ds)
      d ->
        d

denormalizeConstant :: (Data a, Data k, Data (o k), Typeable o) => Name -> Constant Expression a (Type o k) -> Definition a k (Type o k)
denormalizeConstant name =
  \case
    Constant a with (ELambda a1 ps (ELambda _ qs e)) ->
      denormalizeConstant name (Constant a with (ELambda a1 (ps <> qs) e))
    Constant a (With ts _) (ELambda _ ps e) ->
      DFunction name (Function a (With ts (typeOf e)) ps e)
    c@Constant{} ->
      DConstant name c
