{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Compiler.NormalizeObjects where

import Data.Map.Strict (Map)
import Noll.Compiler.Transform
import Noll.Language.Expression (Expression (..))
import Noll.Language.HasType (HasType (..))
import Noll.Language.Module (Module (..))
import Noll.Language.Module.Constant (Constant (..))
import Noll.Language.Module.Definition (Definition (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Language.Trait (Uses (..))
import Noll.Language.Type (Type (..), foldType)

class NormalizeObjectsTransformContext a where
  normalizeObject :: a -> a

instance (NormalizeObjectsTransformContext a) => NormalizeObjectsTransformContext [a] where
  normalizeObject = fmap normalizeObject

instance (NormalizeObjectsTransformContext a) => NormalizeObjectsTransformContext (Map k a) where
  normalizeObject = fmap normalizeObject

instance (Monoid a) => NormalizeObjectsTransformContext (Module a k (Type o k)) where
  normalizeObject =
    \case
      Module p ns d ->
        Module p ns (normalizeObject d)

instance (Monoid a) => NormalizeObjectsTransformContext (Definition a k (Type o k)) where
  normalizeObject =
    \case
      DAnnotation u d ->
        DAnnotation u (normalizeObject d)
      DFunction name (Function a (Uses ts t) ps e) ->
        DConstant name (Constant a (Uses ts (foldType t (typeOf <$> ps))) (flattenLambda (ELambda mempty ps e)))
      d ->
        d

-- instance (Monoid a) => NormalizeObjectsTransformContext (TraitInstance Expression a (Type o k)) where
--  normalizeObject =
--    \case
--      TFunction (Function a (Uses ts t) ps e) ->
--        TConstant (Constant a (Uses ts (foldType t (typeOf <$> ps))) (flattenLambda (ELambda mempty ps e)))
--      t ->
--        t
