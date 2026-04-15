{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PhasePreflight.DetectShadowing (
  passDetectShadowing,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.FreeVars (BoundVars (..))
import Coal.Common.Name (isConstructor)
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Compiler.State (CompilerState (compilerCurrentPath))
import Coal.Language (Choice (..), Clause (..), Expression (..), Guard (..))
import Coal.Language.Definition
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError), forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, traverse_)

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
  detectShadowing :: (Monad m) => Set Name -> e -> CompilerT Metadata m ()

instance (ShadowingContext e) => ShadowingContext [e] where
  detectShadowing = traverse_ . detectShadowing

instance (ShadowingContext e) => ShadowingContext (NonEmpty e) where
  detectShadowing = traverse_ . detectShadowing

instance (Data t) => ShadowingContext (Expression Metadata () t) where
  detectShadowing names =
    \case
      EAnnotation _ _ e ->
        detectShadowing names e
      ELambda a ps e -> do
        newNames <- addNames a (boundIn ps) names
        detectShadowing newNames e
      ELet a gs e1 -> do
        newNames <- addNames a (boundIn gs) names
        detectShadowing names gs
        detectShadowing newNames e1
      ERecursiveLet a p e1 e2 -> do
        newNames <- addNames a (boundIn p) names
        detectShadowing newNames e1
        detectShadowing newNames e2
      ERecord _ _ d e -> do
        traverse_ (detectShadowing names) d
        traverse_ (detectShadowing names) e
      ESelect _ _ e ->
        detectShadowing names e
      EFocus _ _ _ _ e1 e2 -> do
        detectShadowing names e1
        detectShadowing names e2
      EIf _ _ e1 e2 e3 -> do
        detectShadowing names e1
        detectShadowing names e2
        detectShadowing names e3
      EApplication _ _ e1 es -> do
        detectShadowing names e1
        traverse_ (detectShadowing names) es
      EListCons _ _ e1 e2 -> do
        detectShadowing names e1
        detectShadowing names e2
      EMatch _ _ e cs -> do
        detectShadowing names e
        traverse_ (detectShadowing names) cs
      ELambdaMatch _ _ cs -> do
        traverse_ (detectShadowing names) cs
      EFold _ _ es cs -> do
        traverse_ (detectShadowing names) es
        traverse_ (detectShadowing names) cs
      EListLiteral _ _ es ->
        traverse_ (detectShadowing names) es
      ETuple _ _ es ->
        traverse_ (detectShadowing names) es
      _ ->
        pure ()

instance (Data t) => ShadowingContext (Clause Metadata () t) where
  detectShadowing names =
    \case
      EClause a p cs -> do
        newNames <- addNames a (boundIn p) names
        detectShadowing newNames cs

instance (Data t) => ShadowingContext (Choice Expression Metadata () t) where
  detectShadowing names =
    \case
      CPlain _ gs e -> do
        detectShadowing names gs
        detectShadowing names e

instance (Data t) => ShadowingContext (Guard Expression Metadata () t) where
  detectShadowing names =
    \case
      CGuard e ->
        detectShadowing names e

instance (Data t) => ShadowingContext (Binding Expression Metadata () t) where
  detectShadowing names =
    \case
      BPattern a p e -> do
        newNames <- addNames a (boundIn p) names
        detectShadowing newNames e
      BFunction a _ ps e -> do
        newNames <- addNames a (boundIn ps) names
        detectShadowing newNames e

instance ShadowingContext (Module Metadata () ()) where
  detectShadowing names =
    \case
      Module{..} -> do
        setCurrentPathC modulePath
        detectShadowing names moduleDefinitions

instance ShadowingContext (Definition Metadata () ()) where
  detectShadowing names =
    \case
      DFunction loc name def -> do
        newNames <- addNames loc (Set.singleton name) names
        detectShadowing newNames def
      DLet loc name def -> do
        newNames <- addNames loc (Set.singleton name) names
        detectShadowing newNames def
      _ ->
        pure ()

instance ShadowingContext (LetDefinition Metadata () ()) where
  detectShadowing names =
    \case
      LetDefinition{..} ->
        detectShadowing names letDefinitionExpression

instance ShadowingContext (FunctionDefinition Metadata () ()) where
  detectShadowing names =
    \case
      FunctionDefinition{..} -> do
        newNames <- addNames functionDefinitionMetadata (boundIn functionDefinitionPatterns) names
        detectShadowing newNames functionDefinitionExpression

addNames :: (Monad m) => Metadata -> Set Name -> Set Name -> CompilerT Metadata m (Set Name)
addNames loc new names = do
  forM_ curated $
    \name -> do
      path <- gets compilerCurrentPath
      when (name `elem` names) $ do
        tellErrors [Shadowing name (ErrorLocation (principalPath path) loc)]
  pure (curated <> names)
 where
  curated = Set.filter (not . isConstructor) new
