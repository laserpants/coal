{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Compiler.NormalizeObjects (NormalizeObjectsTransformContext (..)) where

import Data.Data (Data, Typeable)
import Data.Map.Strict (Map)
import Lang.Common.List1 (List1)
import Noll.Ast.HasType (foldTypeOf)
import Noll.Compiler.Transform
import Noll.Language.Expression (Expression (..))
import Noll.Language.Trait (With (..))
import Noll.Language.Type (Type (..))
import Noll.Module (Module (..))
import Noll.Module.Constant (Constant (..))
import Noll.Module.Definition (Definition (..))
import Noll.Module.Function (Function (..))

class NormalizeObjectsTransformContext a where
  normalizeObject :: a -> a
  -- TODO
  -- move denormalizeObject here

instance (NormalizeObjectsTransformContext a) => NormalizeObjectsTransformContext [a] where
  normalizeObject = fmap normalizeObject

instance (NormalizeObjectsTransformContext a) => NormalizeObjectsTransformContext (List1 a) where
  normalizeObject = fmap normalizeObject

instance (NormalizeObjectsTransformContext a) => NormalizeObjectsTransformContext (Map k a) where
  normalizeObject = fmap normalizeObject

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => NormalizeObjectsTransformContext (Module a k (Type o k)) where
  normalizeObject =
    \case
      Module p ns d ->
        Module p ns (normalizeObject d)

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
