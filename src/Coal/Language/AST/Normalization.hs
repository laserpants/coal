{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Language.AST.Normalization

AST normalization and denormalization for simplified intermediate representations.

Performs transformations between function definitions and lambda expressions,
converting the syntax tree into a standard form and reversing the process
during denormalization.
-}
module Coal.Language.AST.Normalization (NormalizationContext (..)) where

import Coal.Language
import Coal.Language.AST.Flattening (flattenLambdas)
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

denormalizeConstant :: (Data a, Data k, Data (o k), Typeable o, Ord k) => Name -> LetDefinition a k (Type o k) -> Definition a k (Type o k)
denormalizeConstant name =
  \case
    LetDefinition loc w1 w2 (ELambda a1 ps (ELambda _ qs e)) ->
      denormalizeConstant name (LetDefinition loc w1 w2 (ELambda a1 (ps <> qs) e))
    LetDefinition loc w1 (With ts _) (ELambda _ ps e) ->
      DFunction
        loc
        name
        FunctionDefinition
          { functionDefinitionMetadata = loc
          , functionDefinitionAnnotation = w1
          , functionDefinitionType = With ts (typeOf e)
          , functionDefinitionPatterns = ps
          , functionDefinitionExpression = e
          }
    def@(LetDefinition loc _ _ _) ->
      DLet loc name def

instance (Monoid a, Data a, Data k, Data (o k), Typeable o, Ord k) => NormalizationContext (Module a k (Type o k)) where
  normalizeObject =
    \case
      Module{..} ->
        Module
          { moduleDefinitions = normalizeObject moduleDefinitions
          , ..
          }
  denormalizeObject =
    \case
      Module{..} ->
        Module
          { moduleDefinitions = denormalizeObject moduleDefinitions
          , ..
          }

instance (Monoid a, Data a, Data k, Data (o k), Typeable o, Ord k) => NormalizationContext (Definition a k (Type o k)) where
  normalizeObject =
    \case
      DFunction
        loc
        name
        FunctionDefinition
          { functionDefinitionType = With ts t
          , ..
          } ->
          DLet
            loc
            name
            LetDefinition
              { letDefinitionMetadata =
                  loc
              , letDefinitionAnnotation =
                  functionDefinitionAnnotation
              , letDefinitionType =
                  With ts (foldTypeOf t functionDefinitionPatterns)
              , letDefinitionExpression =
                  flattenLambdas (ELambda mempty functionDefinitionPatterns functionDefinitionExpression)
              }
      DInstance loc InstanceDefinition{..} ->
        DInstance
          loc
          InstanceDefinition
            { instanceDefinitionImplementations =
                normalizeObject instanceDefinitionImplementations
            , ..
            }
      def ->
        def
  denormalizeObject =
    \case
      DLet _ name def ->
        denormalizeConstant name def
      DInstance loc InstanceDefinition{..} ->
        DInstance
          loc
          InstanceDefinition
            { instanceDefinitionImplementations =
                denormalizeObject instanceDefinitionImplementations
            , ..
            }
      def ->
        def
