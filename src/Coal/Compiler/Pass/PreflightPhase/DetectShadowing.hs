{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.DetectShadowing (passDetectShadowing) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.FreeVars (BoundVars (..))
import Coal.Common.Name (isConstructor)
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language (Choice (..), Clause (..), Expression (..), Guard (..))
import Coal.Language.Definition
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError), forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name)

passDetectShadowing :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passDetectShadowing = mapPass $ Pass{runPass = traverse passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
passImpl m = do
  setCurrentModuleC m
  (_, errors) <- listenErrors (detectShadowing mempty m)
  unless (null errors) $
    throwError PreflightFailure
  return m

class ShadowingContext e where
  detectShadowing :: (Monad m) => Set Name -> e -> CompilerT Metadata m e

instance (ShadowingContext e) => ShadowingContext [e] where
  detectShadowing = traverse . detectShadowing

instance (ShadowingContext e) => ShadowingContext (NonEmpty e) where
  detectShadowing = traverse . detectShadowing

instance (Data t) => ShadowingContext (Expression Metadata () t) where
  detectShadowing names =
    \case
      EAnnotation loc t e ->
        EAnnotation loc t <$> detectShadowing names e
      var@EVariable{} -> do
        pure var
      ELambda a ps e -> do
        newNames <- addNames a (boundIn ps) names
        ELambda a ps <$> detectShadowing newNames e
      ELet a gs e1 -> do
        newNames <- addNames a (boundIn gs) names
        ELet a
          <$> detectShadowing names gs
          <*> detectShadowing newNames e1
      ERecursiveLet a p e1 e2 -> do
        newNames <- addNames a (boundIn p) names
        ERecursiveLet a p
          <$> detectShadowing newNames e1
          <*> detectShadowing newNames e2
      ERecord a t d e ->
        ERecord a t
          <$> traverse (detectShadowing names) d
          <*> traverse (detectShadowing names) e
      ESelect a ll e ->
        ESelect a ll <$> detectShadowing names e
      EFocus a field ll1 ll2 e1 e2 -> do
        EFocus a field ll1 ll2 <$> detectShadowing names e1 <*> detectShadowing names e2
      EIf a t e1 e2 e3 ->
        EIf a t
          <$> detectShadowing names e1
          <*> detectShadowing names e2
          <*> detectShadowing names e3
      expr@ELiteral{} ->
        pure expr
      expr@EConstructor{} ->
        pure expr
      EApplication a t e1 es ->
        EApplication a t
          <$> detectShadowing names e1
          <*> traverse (detectShadowing names) es
      EListCons a t e1 e2 ->
        EListCons a t
          <$> detectShadowing names e1
          <*> detectShadowing names e2
      EMatch a t e cs ->
        EMatch a t
          <$> detectShadowing names e
          <*> traverse (detectShadowing names) cs
      ELambdaMatch a t cs ->
        ELambdaMatch a t
          <$> traverse (detectShadowing names) cs
      EFold a t es cs ->
        EFold a t
          <$> traverse (detectShadowing names) es
          <*> traverse (detectShadowing names) cs
      expr@EOperator{} ->
        pure expr
      EListLiteral a t es ->
        EListLiteral a t <$> traverse (detectShadowing names) es
      ETuple a t es ->
        ETuple a t <$> traverse (detectShadowing names) es
      e@EFFICall{} ->
        pure e
      _ ->
        error "Not passImplemented"

instance (Data t) => ShadowingContext (Clause Metadata () t) where
  detectShadowing names =
    \case
      EClause a p cs -> do
        newNames <- addNames a (boundIn p) names
        EClause a p <$> detectShadowing newNames cs

instance (Data t) => ShadowingContext (Choice Expression Metadata () t) where
  detectShadowing names =
    \case
      CPlain a gs e ->
        CPlain a <$> detectShadowing names gs <*> detectShadowing names e

instance (Data t) => ShadowingContext (Guard Expression Metadata () t) where
  detectShadowing names =
    \case
      CGuard e ->
        CGuard <$> detectShadowing names e

instance (Data t) => ShadowingContext (Binding Expression Metadata () t) where
  detectShadowing names =
    \case
      BPattern a p e -> do
        newNames <- addNames a (boundIn p) names
        BPattern a p <$> detectShadowing newNames e
      BFunction a n ps e -> do
        newNames <- addNames a (boundIn ps) names
        BFunction a n ps <$> detectShadowing newNames e

instance ShadowingContext (Module Metadata () ()) where
  detectShadowing names =
    \case
      Module{..} -> do
        setCurrentPathC modulePath
        newModuleDefinitions <- detectShadowing names moduleDefinitions
        return $
          Module
            { moduleDefinitions = newModuleDefinitions
            , ..
            }

instance ShadowingContext (Definition Metadata () ()) where
  detectShadowing names =
    \case
      DFunction loc name def -> do
        newNames <- addNames loc (Set.singleton name) names
        DFunction loc name <$> detectShadowing newNames def
      DLet loc name def -> do
        newNames <- addNames loc (Set.singleton name) names
        DLet loc name <$> detectShadowing newNames def
      o ->
        pure o

instance ShadowingContext (LetDefinition Metadata () ()) where
  detectShadowing names =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- detectShadowing names letDefinitionExpression
        return $
          LetDefinition
            { letDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance ShadowingContext (FunctionDefinition Metadata () ()) where
  detectShadowing names =
    \case
      FunctionDefinition{..} -> do
        newNames <- addNames functionDefinitionMetadata (boundIn functionDefinitionPatterns) names
        newFunctionDefinitionExpression <- detectShadowing newNames functionDefinitionExpression
        return $
          FunctionDefinition
            { functionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

addNames :: (Monad m) => Metadata -> Set Name -> Set Name -> CompilerT Metadata m (Set Name)
addNames loc new names = do
  forM_ new' $
    \name -> do
      path <- gets compilerCurrentPath
      when (name `elem` names) $ do
        tellErrors [Shadowing name (ErrorLocation (principalPath path) loc)]
        throwError PreflightFailure
  pure (new' <> names)
 where
  new' = Set.filter (not . isConstructor) new
