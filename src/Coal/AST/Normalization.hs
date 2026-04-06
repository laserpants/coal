{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.AST.Normalization (NormalizationContext (..)) where

import Coal.AST.Flattening (flattenLambdas)
import Coal.Language.Expression (Expression (..))
import Coal.Language.HasType (HasType (..), foldTypeOf)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Definition (Definition (..))
import Coal.Language.Module.Definition.Constant (ConstantDefinition (..))
import Coal.Language.Module.Definition.Function (FunctionDefinition (..))
import Coal.Language.Module.Definition.Instance (InstanceDefinition (..))
import Coal.Language.Trait (Qualified (..))
import Coal.Language.Type (Type (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
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
      Module{..} ->
        Module modulePath moduleExports (normalizeObject moduleDefinitions)
  denormalizeObject =
    \case
      Module{..} ->
        Module modulePath moduleExports (denormalizeObject moduleDefinitions)

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => NormalizationContext (Definition a k (Type o k)) where
  normalizeObject =
    \case
      DFunction loc name (FunctionDefinition a w1 (With ts t) ps e :| _) _ ->
        DConstant loc name (ConstantDefinition a w1 (With ts (foldTypeOf t ps)) (flattenLambdas (ELambda mempty ps e))) []
      DInstance loc name (InstanceDefinition ts t ds) ->
        DInstance loc name (InstanceDefinition ts t (normalizeObject ds))
      d ->
        d
  denormalizeObject =
    \case
      DConstant _ name c _ ->
        denormalizeConstant name c
      DInstance loc name (InstanceDefinition ts t ds) ->
        DInstance loc name (InstanceDefinition ts t (denormalizeObject ds))
      d ->
        d

denormalizeConstant :: (Data a, Data k, Data (o k), Typeable o) => Name -> ConstantDefinition a (Type o k) -> Definition a k (Type o k)
denormalizeConstant name =
  \case
    ConstantDefinition loc w1 w2 (ELambda a1 ps (ELambda _ qs e)) ->
      denormalizeConstant name (ConstantDefinition loc w1 w2 (ELambda a1 (ps <> qs) e))
    ConstantDefinition loc w1 (With ts _) (ELambda _ ps e) ->
      DFunction loc name (FunctionDefinition loc w1 (With ts (typeOf e)) ps e :| []) []
    def@(ConstantDefinition loc _ _ _) ->
      DConstant loc name def []

denormalizeConstant2 :: (Data a, Data k, Data (o k), Typeable o) => Name -> ProtoLetDefinition a k (Type o k) -> ProtoDefinition a k (Type o k)
denormalizeConstant2 name =
  \case
    ProtoLetDefinition loc w1 w2 (ELambda a1 ps (ELambda _ qs e)) ->
      denormalizeConstant2 name (ProtoLetDefinition loc w1 w2 (ELambda a1 (ps <> qs) e))
    ProtoLetDefinition loc w1 (With ts _) (ELambda _ ps e) ->
      ProtoDFunction
        loc
        name
        ProtoFunctionDefinition
          { protoOfunctionDefinitionMetadata = loc
          , protoOfunctionDefinitionAnnotation = w1
          , protoOfunctionDefinitionType = With ts (typeOf e)
          , protoOfunctionDefinitionPatterns = ps
          , protoOfunctionDefinitionExpression = e
          }
    def@(ProtoLetDefinition loc _ _ _) ->
      ProtoDLet loc name def

--

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => NormalizationContext (ProtoModule a k (Type o k)) where
  normalizeObject =
    \case
      ProtoModule{..} ->
        ProtoModule
          { protoOmoduleDefinitions = normalizeObject protoOmoduleDefinitions
          , ..
          }
  denormalizeObject =
    \case
      ProtoModule{..} ->
        ProtoModule
          { protoOmoduleDefinitions = denormalizeObject protoOmoduleDefinitions
          , ..
          }

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => NormalizationContext (ProtoDefinition a k (Type o k)) where
  normalizeObject =
    \case
      ProtoDFunction
        loc
        name
        ProtoFunctionDefinition
          { protoOfunctionDefinitionType = With ts t
          , ..
          } ->
          ProtoDLet
            loc
            name
            ProtoLetDefinition
              { protoOletDefinitionMetadata =
                  loc
              , protoOletDefinitionAnnotation =
                  protoOfunctionDefinitionAnnotation
              , protoOletDefinitionType =
                  With ts (foldTypeOf t protoOfunctionDefinitionPatterns)
              , protoOletDefinitionExpression =
                  flattenLambdas (ELambda mempty protoOfunctionDefinitionPatterns protoOfunctionDefinitionExpression)
              }
      ProtoDInstance loc ProtoInstanceDefinition{..} ->
        ProtoDInstance
          loc
          ProtoInstanceDefinition
            { protoOinstanceDefinitionImplementations =
                normalizeObject protoOinstanceDefinitionImplementations
            , ..
            }
      d ->
        d
  denormalizeObject =
    \case
      ProtoDLet _ name def ->
        denormalizeConstant2 name def
      ProtoDInstance loc ProtoInstanceDefinition{..} ->
        ProtoDInstance
          loc
          ProtoInstanceDefinition
            { protoOinstanceDefinitionImplementations =
                denormalizeObject protoOinstanceDefinitionImplementations
            , ..
            }
      d ->
        d
