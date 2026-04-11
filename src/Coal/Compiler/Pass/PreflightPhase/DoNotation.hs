{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.DoNotation (passDoNotation) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

passDoNotation :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passDoNotation = mapPass $ Pass{runPass = traverse impl}

impl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
impl = desugarDoNotation

class TransformContext e where
  desugarDoNotation :: (Monad m) => e -> CompilerT Metadata m e

instance (TransformContext a) => TransformContext (Maybe a) where
  desugarDoNotation = traverse desugarDoNotation

instance (Data a, Monoid a) => TransformContext (Module a () ()) where
  desugarDoNotation =
    \case
      Module{..} -> do
        newModuleDefinitions <- traverse desugarDoNotation protoOmoduleDefinitions
        return $
          Module
            { protoOmoduleDefinitions = newModuleDefinitions
            , ..
            }

instance (Data a, Monoid a) => TransformContext (Definition a () ()) where
  desugarDoNotation =
    \case
      DFunction loc name def ->
        DFunction loc name <$> desugarDoNotation def
      DLet loc name def ->
        DLet loc name <$> desugarDoNotation def
      DInstance loc def ->
        DInstance loc <$> desugarDoNotation def
      DFold loc name def ->
        DFold loc name <$> desugarDoNotation def
      o ->
        pure o

instance (Data a, Monoid a) => TransformContext (InstanceDefinition a () ()) where
  desugarDoNotation =
    \case
      InstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- traverse desugarDoNotation protoOinstanceDefinitionImplementations
        return $
          InstanceDefinition
            { protoOinstanceDefinitionImplementations = newInstanceDefinitionImplementations
            , ..
            }

instance (Data a, Monoid a) => TransformContext (FoldDefinition a () ()) where
  desugarDoNotation =
    \case
      FoldDefinition{..} -> do
        newFoldDefinitionClauses <- traverse desugarDoNotation protoOfoldDefinitionClauses
        return $
          FoldDefinition
            { protoOfoldDefinitionClauses = newFoldDefinitionClauses
            , ..
            }

instance (Data a, Monoid a) => TransformContext (FunctionDefinition a () ()) where
  desugarDoNotation =
    \case
      FunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- desugarDoNotation protoOfunctionDefinitionExpression
        return $
          FunctionDefinition
            { protoOfunctionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

instance (Data a, Monoid a) => TransformContext (LetDefinition a () ()) where
  desugarDoNotation =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- desugarDoNotation protoOletDefinitionExpression
        return $
          LetDefinition
            { protoOletDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance (Data a, Monoid a) => TransformContext (Expression a () ()) where
  desugarDoNotation =
    \case
      EDoBlock _ es ->
        pure $ foldr go e' es'
       where
        (e', es') = normalize es
        bind e1 e2 = EApplication mempty () (EVariable mempty (Label () "bind")) (e1 :| [e2])
        go (p, e) e2 = bind e (ELambda mempty (p :| []) e2)
      e ->
        descendM desugarDoNotation e

normalize :: (Monoid a) => NonEmpty (Pattern a () (), Expression a () ()) -> (Expression a () (), NonEmpty (Pattern a () (), Expression a () ()))
normalize es =
  case lst of
    (PAny _ (), e) ->
      (e, NonEmpty.fromList $ NonEmpty.init es)
    _ ->
      (EApplication mempty () (EVariable mempty (Label () "pure")) (ELiteral mempty LUnit :| []), es)
 where
  lst = NonEmpty.last es

instance (Data a, Monoid a) => TransformContext (Clause a () ()) where
  desugarDoNotation =
    \case
      EClause a p cs ->
        EClause a p <$> traverse desugarDoNotation cs

instance (Data a, Monoid a) => TransformContext (Choice Expression a () ()) where
  desugarDoNotation =
    \case
      CPlain a gs e ->
        CPlain a <$> traverse desugarDoNotation gs <*> desugarDoNotation e

instance (Data a, Monoid a) => TransformContext (Guard Expression a () ()) where
  desugarDoNotation =
    \case
      CGuard e ->
        CGuard <$> desugarDoNotation e
