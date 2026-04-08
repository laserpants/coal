{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.DoNotation (passDoNotation) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

passDoNotation :: (MonadIO m) => Pass Metadata m [BuildUnit (ProtoModule Metadata () ())] [BuildUnit (ProtoModule Metadata () ())]
passDoNotation = mapPass $ Pass{runPass = traverse impl}

impl :: (MonadIO m) => ProtoModule Metadata () () -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata () ())
impl = desugarDoNotation 

class TransformContext e where
  desugarDoNotation :: (Monad m) => e -> CompilerT a (ProtoCompilerT m Metadata) e

instance (TransformContext a) => TransformContext (Maybe a) where
  desugarDoNotation = traverse desugarDoNotation

instance (Data a, Monoid a) => TransformContext (ProtoModule a () ()) where
  desugarDoNotation =
    \case
      ProtoModule{..} -> do
        newModuleDefinitions <- traverse desugarDoNotation protoOmoduleDefinitions
        return $
          ProtoModule
            { protoOmoduleDefinitions = newModuleDefinitions
            , ..
            }

instance (Data a, Monoid a) => TransformContext (ProtoDefinition a () ()) where
  desugarDoNotation =
    \case
      ProtoDFunction loc name def ->
        ProtoDFunction loc name <$> desugarDoNotation def
      ProtoDLet loc name def ->
        ProtoDLet loc name <$> desugarDoNotation def
      ProtoDInstance loc def ->
        ProtoDInstance loc <$> desugarDoNotation def
      ProtoDFold loc name def ->
        ProtoDFold loc name <$> desugarDoNotation def
      o ->
        pure o

instance (Data a, Monoid a) => TransformContext (ProtoInstanceDefinition a () ()) where
  desugarDoNotation =
    \case
      ProtoInstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- traverse desugarDoNotation protoOinstanceDefinitionImplementations
        return $
          ProtoInstanceDefinition
            { protoOinstanceDefinitionImplementations = newInstanceDefinitionImplementations
            , ..
            }

instance (Data a, Monoid a) => TransformContext (ProtoFoldDefinition a () ()) where
  desugarDoNotation =
    \case
      ProtoFoldDefinition{..} -> do
        newFoldDefinitionClauses <- traverse desugarDoNotation protoOfoldDefinitionClauses
        return $
          ProtoFoldDefinition
            { protoOfoldDefinitionClauses = newFoldDefinitionClauses
            , ..
            }

instance (Data a, Monoid a) => TransformContext (ProtoFunctionDefinition a () ()) where
  desugarDoNotation =
    \case
      ProtoFunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- desugarDoNotation protoOfunctionDefinitionExpression
        return $
          ProtoFunctionDefinition
            { protoOfunctionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

instance (Data a, Monoid a) => TransformContext (ProtoLetDefinition a () ()) where
  desugarDoNotation =
    \case
      ProtoLetDefinition{..} -> do
        newLetDefinitionExpression <- desugarDoNotation protoOletDefinitionExpression
        return $
          ProtoLetDefinition
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
