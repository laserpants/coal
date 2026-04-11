{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.AST.Normalization (NormalizationContext (..)) where

import Coal.AST.Flattening (flattenLambdas)
import Coal.Language.Definition
import Coal.Language.Expression (Expression (..))
import Coal.Language.HasType (HasType (..), foldTypeOf)
import Coal.Language.Module
import Coal.Language.Trait (Qualified (..))
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

denormalizeConstant :: (Data a, Data k, Data (o k), Typeable o) => Name -> LetDefinition a k (Type o k) -> Definition a k (Type o k)
denormalizeConstant name =
  \case
    LetDefinition loc w1 w2 (ELambda a1 ps (ELambda _ qs e)) ->
      denormalizeConstant name (LetDefinition loc w1 w2 (ELambda a1 (ps <> qs) e))
    LetDefinition loc w1 (With ts _) (ELambda _ ps e) ->
      DFunction
        loc
        name
        FunctionDefinition
          { protoOfunctionDefinitionMetadata = loc
          , protoOfunctionDefinitionAnnotation = w1
          , protoOfunctionDefinitionType = With ts (typeOf e)
          , protoOfunctionDefinitionPatterns = ps
          , protoOfunctionDefinitionExpression = e
          }
    def@(LetDefinition loc _ _ _) ->
      DLet loc name def

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => NormalizationContext (Module a k (Type o k)) where
  normalizeObject =
    \case
      Module{..} ->
        Module
          { protoOmoduleDefinitions = normalizeObject protoOmoduleDefinitions
          , ..
          }
  denormalizeObject =
    \case
      Module{..} ->
        Module
          { protoOmoduleDefinitions = denormalizeObject protoOmoduleDefinitions
          , ..
          }

instance (Monoid a, Data a, Data k, Data (o k), Typeable o) => NormalizationContext (Definition a k (Type o k)) where
  normalizeObject =
    \case
      DFunction
        loc
        name
        FunctionDefinition
          { protoOfunctionDefinitionType = With ts t
          , ..
          } ->
          DLet
            loc
            name
            LetDefinition
              { protoOletDefinitionMetadata =
                  loc
              , protoOletDefinitionAnnotation =
                  protoOfunctionDefinitionAnnotation
              , protoOletDefinitionType =
                  With ts (foldTypeOf t protoOfunctionDefinitionPatterns)
              , protoOletDefinitionExpression =
                  flattenLambdas (ELambda mempty protoOfunctionDefinitionPatterns protoOfunctionDefinitionExpression)
              }
      DInstance loc InstanceDefinition{..} ->
        DInstance
          loc
          InstanceDefinition
            { protoOinstanceDefinitionImplementations =
                normalizeObject protoOinstanceDefinitionImplementations
            , ..
            }
      d ->
        d
  denormalizeObject =
    \case
      DLet _ name def ->
        denormalizeConstant name def
      DInstance loc InstanceDefinition{..} ->
        DInstance
          loc
          InstanceDefinition
            { protoOinstanceDefinitionImplementations =
                denormalizeObject protoOinstanceDefinitionImplementations
            , ..
            }
      d ->
        d
