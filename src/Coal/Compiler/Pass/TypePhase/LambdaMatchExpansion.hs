{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.LambdaMatchExpansion (LambdaMatchExpressionTransform (..), passLambdaMatchExpansion) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language.Definition
import Coal.Language.Expression
import Coal.Language.Module (Module (..))
import Coal.Language.Pattern
import Coal.Language.Type.Kind
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary)

passLambdaMatchExpansion :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind ()) (Module a Kind ())
passLambdaMatchExpansion = Pass{runPass = lambdaMatchExpressionTransform}

class LambdaMatchExpressionTransform t where
  lambdaMatchExpressionTransform :: (Monad m) => t -> CompilerT a m t

instance (LambdaMatchExpressionTransform a) => LambdaMatchExpressionTransform [a] where
  lambdaMatchExpressionTransform = traverse lambdaMatchExpressionTransform

instance (LambdaMatchExpressionTransform a) => LambdaMatchExpressionTransform (NonEmpty a) where
  lambdaMatchExpressionTransform = traverse lambdaMatchExpressionTransform

instance (LambdaMatchExpressionTransform a) => LambdaMatchExpressionTransform (Dictionary a) where
  lambdaMatchExpressionTransform = traverse lambdaMatchExpressionTransform

instance (Monoid a, Data a) => LambdaMatchExpressionTransform (Module a Kind ()) where
  lambdaMatchExpressionTransform =
    \case
      Module{..} ->
        Module protoOmodulePath protoOmoduleExportList
          <$> lambdaMatchExpressionTransform protoOmoduleDefinitions

instance (Monoid a, Data a) => LambdaMatchExpressionTransform (Definition a Kind ()) where
  lambdaMatchExpressionTransform =
    \case
      DFunction loc name def ->
        DFunction loc name <$> lambdaMatchExpressionTransform def
      DLet loc name def ->
        DLet loc name <$> lambdaMatchExpressionTransform def
      o ->
        pure o

instance (Monoid a, Data a) => LambdaMatchExpressionTransform (LetDefinition a Kind ()) where
  lambdaMatchExpressionTransform =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- lambdaMatchExpressionTransform protoOletDefinitionExpression
        return $
          LetDefinition
            { protoOletDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance (Monoid a, Data a) => LambdaMatchExpressionTransform (FunctionDefinition a Kind ()) where
  lambdaMatchExpressionTransform =
    \case
      FunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- lambdaMatchExpressionTransform protoOfunctionDefinitionExpression
        return $
          FunctionDefinition
            { protoOfunctionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

instance (Monoid a, Data a) => LambdaMatchExpressionTransform (Expression a Kind ()) where
  lambdaMatchExpressionTransform =
    transformM $
      \case
        ELambdaMatch _ _ clauses ->
          pure $
            ELambda
              mempty
              (PVariable mempty (Label () "$lambda_match") :| [])
              ( EMatch
                  mempty
                  ()
                  (EVariable mempty (Label () "$lambda_match"))
                  clauses
              )
        e ->
          pure e
