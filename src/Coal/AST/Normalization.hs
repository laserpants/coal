{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.AST.Normalization (NormalizationContext (..)) where

import Coal.AST.Flattening (flattenLambdas)
import Coal.Language.Expression (Expression (..))
import Coal.Language.HasType (HasType (..), foldTypeOf)
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

denormalizeConstant :: (Data a, Data k, Data (o k), Typeable o) => Name -> ProtoLetDefinition a k (Type o k) -> ProtoDefinition a k (Type o k)
denormalizeConstant name =
  \case
    ProtoLetDefinition loc w1 w2 (ELambda a1 ps (ELambda _ qs e)) ->
      denormalizeConstant name (ProtoLetDefinition loc w1 w2 (ELambda a1 (ps <> qs) e))
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
        denormalizeConstant name def
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
