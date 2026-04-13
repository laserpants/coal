{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.DesugarDoNotation (
  passDesugarDoNotation,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.AST.Shorthand
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

passDesugarDoNotation :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passDesugarDoNotation = mapPass $ Pass{runPass = traverse passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
passImpl = desugarDoNotation

class DoNotationContext e where
  desugarDoNotation :: (Monad m) => e -> CompilerT Metadata m e

instance (DoNotationContext a) => DoNotationContext (Maybe a) where
  desugarDoNotation = traverse desugarDoNotation

instance (Data a, Monoid a) => DoNotationContext (Module a () ()) where
  desugarDoNotation =
    \case
      Module{..} -> do
        newModuleDefinitions <- traverse desugarDoNotation moduleDefinitions
        return $
          Module
            { moduleDefinitions = newModuleDefinitions
            , ..
            }

instance (Data a, Monoid a) => DoNotationContext (Definition a () ()) where
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

instance (Data a, Monoid a) => DoNotationContext (InstanceDefinition a () ()) where
  desugarDoNotation =
    \case
      InstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- traverse desugarDoNotation instanceDefinitionImplementations
        return $
          InstanceDefinition
            { instanceDefinitionImplementations = newInstanceDefinitionImplementations
            , ..
            }

instance (Data a, Monoid a) => DoNotationContext (FoldDefinition a () ()) where
  desugarDoNotation =
    \case
      FoldDefinition{..} -> do
        newFoldDefinitionClauses <- traverse desugarDoNotation foldDefinitionClauses
        return $
          FoldDefinition
            { foldDefinitionClauses = newFoldDefinitionClauses
            , ..
            }

instance (Data a, Monoid a) => DoNotationContext (FunctionDefinition a () ()) where
  desugarDoNotation =
    \case
      FunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- desugarDoNotation functionDefinitionExpression
        return $
          FunctionDefinition
            { functionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

instance (Data a, Monoid a) => DoNotationContext (LetDefinition a () ()) where
  desugarDoNotation =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- desugarDoNotation letDefinitionExpression
        return $
          LetDefinition
            { letDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance (Data a, Monoid a) => DoNotationContext (Expression a () ()) where
  desugarDoNotation =
    \case
      EDoBlock _ es ->
        pure (foldr go e' es')
       where
        (e', es') = normalize es
        bind e1 e2 = applicationE (varE "bind") (e1 :| [e2])
        go (p, e) e2 = bind e (lambdaE (p :| []) e2)
      e ->
        descendM desugarDoNotation e

instance (Data a, Monoid a) => DoNotationContext (Clause a () ()) where
  desugarDoNotation =
    \case
      EClause a p cs ->
        EClause a p <$> traverse desugarDoNotation cs

instance (Data a, Monoid a) => DoNotationContext (Choice Expression a () ()) where
  desugarDoNotation =
    \case
      CPlain a gs e ->
        CPlain a <$> traverse desugarDoNotation gs <*> desugarDoNotation e

instance (Data a, Monoid a) => DoNotationContext (Guard Expression a () ()) where
  desugarDoNotation =
    \case
      CGuard e ->
        CGuard <$> desugarDoNotation e

normalize :: (Monoid a) => NonEmpty (Pattern a () (), Expression a () ()) -> (Expression a () (), NonEmpty (Pattern a () (), Expression a () ()))
normalize es =
  case NonEmpty.last es of
    (PAny _ (), e) ->
      (e, NonEmpty.fromList $ NonEmpty.init es)
    _ ->
      (applicationE (varE "pure") (ELiteral mempty LUnit :| []), es)
