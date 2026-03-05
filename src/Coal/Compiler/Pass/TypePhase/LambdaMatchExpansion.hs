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
import Coal.Compiler.Stack (CompilerT)
import Coal.Language.Expression
import Coal.Language.Pattern
import Coal.Language.Type.Kind
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary)

passLambdaMatchExpansion :: (Monad m, Monoid a, Data a) => Pass a m (ProtoModule a Kind ()) (ProtoModule a Kind ())
passLambdaMatchExpansion = Pass{runPass = lambdaMatchExpressionTransform}

class LambdaMatchExpressionTransform t where
  lambdaMatchExpressionTransform :: (Monad m) => t -> CompilerT a (ProtoCompilerT m a) t

instance (LambdaMatchExpressionTransform a) => LambdaMatchExpressionTransform [a] where
  lambdaMatchExpressionTransform = traverse lambdaMatchExpressionTransform

instance (LambdaMatchExpressionTransform a) => LambdaMatchExpressionTransform (NonEmpty a) where
  lambdaMatchExpressionTransform = traverse lambdaMatchExpressionTransform

instance (LambdaMatchExpressionTransform a) => LambdaMatchExpressionTransform (Dictionary a) where
  lambdaMatchExpressionTransform = traverse lambdaMatchExpressionTransform

instance (Monoid a, Data a) => LambdaMatchExpressionTransform (ProtoModule a Kind ()) where
  lambdaMatchExpressionTransform =
    \case
      ProtoModule{..} ->
        ProtoModule protoOmodulePath protoOmoduleExportList
          <$> lambdaMatchExpressionTransform protoOmoduleDefinitions

instance (Monoid a, Data a) => LambdaMatchExpressionTransform (ProtoDefinition a Kind ()) where
  lambdaMatchExpressionTransform =
    \case
      ProtoDFunction loc name def ->
        ProtoDFunction loc name <$> lambdaMatchExpressionTransform def
      ProtoDLet loc name def ->
        ProtoDLet loc name <$> lambdaMatchExpressionTransform def
      o ->
        pure o

instance (Monoid a, Data a) => LambdaMatchExpressionTransform (ProtoLetDefinition a Kind ()) where
  lambdaMatchExpressionTransform =
    \case
      ProtoLetDefinition{..} -> do
        newLetDefinitionExpression <- lambdaMatchExpressionTransform protoOletDefinitionExpression
        return $
          ProtoLetDefinition
            { protoOletDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance (Monoid a, Data a) => LambdaMatchExpressionTransform (ProtoFunctionDefinition a Kind ()) where
  lambdaMatchExpressionTransform =
    \case
      ProtoFunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- lambdaMatchExpressionTransform protoOfunctionDefinitionExpression
        return $
          ProtoFunctionDefinition
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
