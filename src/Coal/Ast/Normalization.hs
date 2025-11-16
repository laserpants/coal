{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Coal.AST.Normalization (NormalizationContext (..)) where

import Coal.AST.Flattening (flattenLambda)
import Coal.Language.Expression (Expression (..))
import Coal.Language.HasType (HasType (..), foldTypeOf)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Definition (Definition (..))
import Coal.Language.Module.Definition.Constant (ConstantDef (..))
import Coal.Language.Module.Definition.Function (FunctionDef (..))
import Coal.Language.Module.Definition.Instance (InstanceDef (..))
import Coal.Language.Trait (With (..))
import Coal.Language.Type (Type (..))
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Extras (Name)

class NormalizationContext a where
  normalizeObject :: a -> a
  denormalizeObject :: a -> a

instance (NormalizationContext a) => NormalizationContext [a] where
  normalizeObject = fmap normalizeObject
  denormalizeObject = fmap denormalizeObject

instance (NormalizationContext a) => NormalizationContext (NonEmpty a) where
  normalizeObject = fmap normalizeObject
  denormalizeObject = fmap denormalizeObject

instance (NormalizationContext a) => NormalizationContext (Map k a) where
  normalizeObject = fmap normalizeObject
  denormalizeObject = fmap denormalizeObject

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => NormalizationContext (Module a k (Type o k)) where
  normalizeObject =
    \case
      Module p ns d ->
        Module p ns (normalizeObject d)
  denormalizeObject =
    \case
      Module p ns d ->
        Module p ns (denormalizeObject d)

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => NormalizationContext (Definition a k (Type o k)) where
  normalizeObject =
    \case
      DFunction loc name (FunctionDef a w1 (With ts t) ps e :| _) _ ->
        DConstant loc name (ConstantDef a w1 (With ts (foldTypeOf t ps)) (flattenLambda (ELambda mempty ps e))) []
      DInstance loc name (InstanceDef ts t ds) ->
        DInstance loc name (InstanceDef ts t (normalizeObject ds))
      d ->
        d
  denormalizeObject =
    \case
      DConstant _ name c _ ->
        denormalizeConstant name c
      DInstance loc name (InstanceDef ts t ds) ->
        DInstance loc name (InstanceDef ts t (denormalizeObject ds))
      d ->
        d

denormalizeConstant :: (Data a, Data k, Data (o k), Typeable o) => Name -> ConstantDef a (Type o k) -> Definition a k (Type o k)
denormalizeConstant name =
  \case
    ConstantDef loc w1 w2 (ELambda a1 ps (ELambda _ qs e)) ->
      denormalizeConstant name (ConstantDef loc w1 w2 (ELambda a1 (ps <> qs) e))
    ConstantDef loc w1 (With ts _) (ELambda _ ps e) ->
      DFunction loc name (FunctionDef loc w1 (With ts (typeOf e)) ps e :| []) []
    def@(ConstantDef loc _ _ _) ->
      DConstant loc name def []
