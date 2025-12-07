{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.ShadowingRule (
  RuleContext (..),
  passShadowingRule,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.FreeVars (BoundVars (..))
import Coal.Common.Name (isConstructor)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language (Choice (..), Clause (..), Expression (..), Guard (..), Kind (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Module
import Control.Monad.Except (MonadError (throwError), forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name)

passShadowingRule :: (MonadIO m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
passShadowingRule =
  mapPass $
    Pass
      { passName = "ShadowingRule"
      , runPass = detectShadowing mempty
      }

class RuleContext e where
  detectShadowing :: (Monad m) => Set Name -> e -> CompilerT Metadata m e

instance (RuleContext e) => RuleContext [e] where
  detectShadowing = traverse . detectShadowing

instance (RuleContext e) => RuleContext (NonEmpty e) where
  detectShadowing = traverse . detectShadowing

instance (Data t) => RuleContext (Expression Metadata t) where
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
      EFocus field ll1 ll2 e1 e2 -> do
        EFocus field ll1 ll2 <$> detectShadowing names e1 <*> detectShadowing names e2
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
      ELambdaMatch a t cs me ->
        ELambdaMatch a t
          <$> traverse (detectShadowing names) cs
          <*> traverse (detectShadowing names) me
      EFold a t es cs me ->
        EFold a t
          <$> traverse (detectShadowing names) es
          <*> traverse (detectShadowing names) cs
          <*> traverse (detectShadowing names) me
      ECodataSelect a ll e1 e2 ->
        ECodataSelect a ll
          <$> traverse (detectShadowing names) e1
          <*> traverse (detectShadowing names) e2
      expr@EUnaryOperator{} ->
        pure expr
      expr@EBinaryOperator{} ->
        pure expr
      EListLiteral a t es ->
        EListLiteral a t <$> traverse (detectShadowing names) es
      ETuple a t es ->
        ETuple a t <$> traverse (detectShadowing names) es
      e@EFFICall{} ->
        pure e
      _ ->
        error "TODO"

instance (Data t) => RuleContext (Clause Metadata t) where
  detectShadowing names =
    \case
      EClause a p cs -> do
        names' <- addNames a (boundIn p) names
        EClause a p <$> detectShadowing names' cs

instance (Data t) => RuleContext (Choice Expression Metadata t) where
  detectShadowing names =
    \case
      CPlain a gs e ->
        CPlain a <$> detectShadowing names gs <*> detectShadowing names e

instance (Data t) => RuleContext (Guard Expression Metadata t) where
  detectShadowing names =
    \case
      CGuard e ->
        CGuard <$> detectShadowing names e

instance (Data t) => RuleContext (Binding Expression Metadata t) where
  detectShadowing names =
    \case
      BPattern a p e -> do
        names' <- addNames a (boundIn p) names
        BPattern a p <$> detectShadowing names' e
      BFunction a n ps e -> do
        names' <- addNames a (boundIn ps) names
        BFunction a n ps <$> detectShadowing names' e

instance (Data t) => RuleContext (Module Metadata Kind t) where
  detectShadowing names =
    \case
      Module p ns o -> do
        setCompilerCurrentModuleC p
        Module p ns <$> detectShadowing names o

instance (Data t) => RuleContext (Definition Metadata k t) where
  detectShadowing names =
    \case
      DFunction loc name f fs -> do
        names' <- addNames loc (Set.singleton name) names
        DFunction loc name
          <$> detectShadowing names' f
          <*> detectShadowing names' fs
      DConstant loc name g fs -> do
        names' <- addNames loc (Set.singleton name) names
        DConstant loc name
          <$> detectShadowing names' g
          <*> detectShadowing names' fs
      o ->
        pure o

instance (Data t) => RuleContext (ConstantDefinition Metadata t) where
  detectShadowing names =
    \case
      ConstantDefinition a u w e ->
        ConstantDefinition a u w <$> detectShadowing names e

instance (Data t) => RuleContext (FunctionDefinition Metadata t) where
  detectShadowing names =
    \case
      FunctionDefinition a u w ps e -> do
        names' <- addNames a (boundIn ps) names
        FunctionDefinition a u w ps <$> detectShadowing names' e

addNames :: (Monad m) => Metadata -> Set Name -> Set Name -> CompilerT Metadata m (Set Name)
addNames loc new names = do
  forM_ new' $
    \name -> do
      path <- gets compilerCurrentModule
      when (name `elem` names) $ do
        tellErrors [Shadowing name (ErrorLocation (principalPath path) loc)]
        throwError PreflightFailure
  pure (new' <> names)
 where
  new' = Set.filter (not . isConstructor) new
