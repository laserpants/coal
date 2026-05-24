{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhasePreflight.DesugarDoNotation

Desugar do-notation into explicit bind operations.

This pass transforms do-notation syntax into explicit monadic bind (>>=)
operations. Do-notation is a convenient imperative-style syntax for monadic
computations that is desugared into the underlying monadic operations.

For example, this do-expression:

@
do {
  x <- action1();
  y <- action2(x);
  pure(x + y)
}
@

desugars into:

@
action1() >>= fn(x) =>
  action2(x) >>= fn(y) =>
    pure(x + y)
@

This transformation converts the syntactic sugar into the explicit monadic
operations that the rest of the compiler pipeline can process.
-}
module Coal.Compiler.Pass.PhasePreflight.DesugarDoNotation (
  passDesugarDoNotation,
) where

import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.AST.Builders
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Data)
import Data.Foldable (foldr')
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

{- | Do-notation desugaring pass.

Transform do-notation syntax into monadic bind (>>=) operations.
-}
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
        case es of
          (p, e2) :| [] -> do
            pure (lambdaE (p :| []) e2)
          _ ->
            pure (foldr' go e' es')
       where
        (e', es') = normalize es
        bind e1 e2 = applicationE (varE "bind") (e1 :| [e2])
        go (p, e) e2 = bind e (lambdaE (p :| []) e2)
      e ->
        descendM desugarDoNotation e

instance (Data a, Monoid a) => DoNotationContext (Clause a () ()) where
  desugarDoNotation =
    \case
      EClause{..} ->
        EClause clauseMetadata clausePattern
          <$> traverse desugarDoNotation clauseChoices

instance (Data a, Monoid a) => DoNotationContext (Choice Expression a () ()) where
  desugarDoNotation =
    \case
      CPlain{..} ->
        CPlain choiceMetadata
          <$> traverse desugarDoNotation choiceGuards
          <*> desugarDoNotation choiceExpression

instance (Data a, Monoid a) => DoNotationContext (Guard Expression a () ()) where
  desugarDoNotation =
    \case
      CGuard{..} ->
        CGuard <$> desugarDoNotation guardExpression

normalize :: (Monoid a) => NonEmpty (Pattern a () (), Expression a () ()) -> (Expression a () (), NonEmpty (Pattern a () (), Expression a () ()))
normalize es =
  case NonEmpty.last es of
    (PAny _ (), e) ->
      (e, NonEmpty.fromList $ NonEmpty.init es)
    _ ->
      (applicationE (varE "pure") (literalE LUnit :| []), es)
