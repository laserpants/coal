{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.ShadowingRule (passShadowingRule) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.FreeVars (BoundVars (..))
import Coal.Common.Name (isConstructor)
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language (Choice (..), Clause (..), Expression (..), Guard (..), Kind (..))
import Coal.Language.Definition
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad.Except (MonadError (throwError), forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Control.Monad.Trans (lift)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name)

passShadowingRule :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passShadowingRule = mapPass $ Pass{runPass = traverse impl}

impl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
impl mm = do
  --  let mm = toModule [] m
  detectShadowing mempty mm

class RuleContext e where
  detectShadowing :: (Monad m) => Set Name -> e -> CompilerT Metadata m e

instance (RuleContext e) => RuleContext [e] where
  detectShadowing = traverse . detectShadowing

instance (RuleContext e) => RuleContext (NonEmpty e) where
  detectShadowing = traverse . detectShadowing

instance (Data t) => RuleContext (Expression Metadata () t) where
  detectShadowing names =
    \case
      EAnnotation loc t e ->
        EAnnotation loc t <$> detectShadowing names e
      var@EVariable{} -> do
        pure var
      ELambda a ps e -> do
        names' <- addNames a (boundIn ps) names
        ELambda a ps <$> detectShadowing names' e
      ELet a gs e1 -> do
        names' <- addNames a (boundIn gs) names
        ELet a
          <$> detectShadowing names gs
          <*> detectShadowing names' e1
      ERecursiveLet a p e1 e2 -> do
        names' <- addNames a (boundIn p) names
        ERecursiveLet a p
          <$> detectShadowing names' e1
          <*> detectShadowing names' e2
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
        error "Not implemented"

instance (Data t) => RuleContext (Clause Metadata () t) where
  detectShadowing names =
    \case
      EClause a p cs -> do
        names' <- addNames a (boundIn p) names
        EClause a p <$> detectShadowing names' cs

instance (Data t) => RuleContext (Choice Expression Metadata () t) where
  detectShadowing names =
    \case
      CPlain a gs e ->
        CPlain a <$> detectShadowing names gs <*> detectShadowing names e

instance (Data t) => RuleContext (Guard Expression Metadata () t) where
  detectShadowing names =
    \case
      CGuard e ->
        CGuard <$> detectShadowing names e

instance (Data t) => RuleContext (Binding Expression Metadata () t) where
  detectShadowing names =
    \case
      BPattern a p e -> do
        names' <- addNames a (boundIn p) names
        BPattern a p <$> detectShadowing names' e
      BFunction a n ps e -> do
        names' <- addNames a (boundIn ps) names
        BFunction a n ps <$> detectShadowing names' e

instance RuleContext (Module Metadata () ()) where
  detectShadowing names =
    \case
      Module{..} -> do
        setCurrentPathC protoOmodulePath
        newModuleDefinitions <- detectShadowing names protoOmoduleDefinitions
        return $
          Module
            { protoOmoduleDefinitions = newModuleDefinitions
            , ..
            }

instance RuleContext (Definition Metadata () ()) where
  detectShadowing names =
    \case
      DFunction loc name def -> do
        names' <- addNames loc (Set.singleton name) names
        DFunction loc name <$> detectShadowing names' def
      DLet loc name def -> do
        names' <- addNames loc (Set.singleton name) names
        DLet loc name <$> detectShadowing names' def
      o ->
        pure o

instance RuleContext (LetDefinition Metadata () ()) where
  detectShadowing names =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- detectShadowing names protoOletDefinitionExpression
        return $
          LetDefinition
            { protoOletDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance RuleContext (FunctionDefinition Metadata () ()) where
  detectShadowing names =
    \case
      FunctionDefinition{..} -> do
        names' <- addNames protoOfunctionDefinitionMetadata (boundIn protoOfunctionDefinitionPatterns) names
        newFunctionDefinitionExpression <- detectShadowing names' protoOfunctionDefinitionExpression
        return $
          FunctionDefinition
            { protoOfunctionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

addNames :: (Monad m) => Metadata -> Set Name -> Set Name -> CompilerT Metadata m (Set Name)
addNames loc new names = do
  forM_ new' $
    \name -> do
      path <- gets protoOcompilerCurrentPath
      when (name `elem` names) $ do
        tellErrors [Shadowing name (ErrorLocation (principalPath path) loc)]
        throwError PreflightFailure
  pure (new' <> names)
 where
  new' = Set.filter (not . isConstructor) new
