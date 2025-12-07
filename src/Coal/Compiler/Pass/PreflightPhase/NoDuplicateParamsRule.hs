{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.NoDuplicateParamsRule (
  passNoDuplicateParamsRule,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language (Kind (..))
import Coal.Language.Module
import Coal.Language.Pattern (Pattern (..))
import Control.Monad.Except
import Control.Monad.State (StateT, evalStateT, get, gets, modify)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, traverse_)

passNoDuplicateParamsRule :: (MonadIO m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
passNoDuplicateParamsRule =
  mapPass $
    Pass
      { passName = "NoDuplicateParamsRule"
      , runPass = pass
      }

pass :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
pass m@(Module p _ _) = do
  setCompilerCurrentModuleC p
  detectNoDuplicateParams m
  pure m

class RuleContext e where
  detectNoDuplicateParams :: (Monad m) => e -> CompilerT Metadata m ()

instance (RuleContext e) => RuleContext [e] where
  detectNoDuplicateParams = traverse_ detectNoDuplicateParams

instance (RuleContext e) => RuleContext (NonEmpty e) where
  detectNoDuplicateParams = traverse_ detectNoDuplicateParams

instance (Data t) => RuleContext (Module Metadata Kind t) where
  detectNoDuplicateParams =
    \case
      Module _ _ o ->
        traverse_ detectNoDuplicateParams o

instance (Data t) => RuleContext (Definition Metadata k t) where
  detectNoDuplicateParams =
    \case
      DFunction _ _ f fs -> do
        detectNoDuplicateParams f
        traverse_ detectNoDuplicateParams fs
      _ ->
        pure ()

instance RuleContext (FunctionDefinition Metadata t) where
  detectNoDuplicateParams =
    \case
      FunctionDefinition _ _ _ ps _ -> do
        evalStateT (traverse_ checkPattern ps) mempty

checkPattern :: (Monad m) => Pattern Metadata t -> StateT (Set Name) (CompilerT Metadata m) ()
checkPattern =
  \case
    PAnnotation _ _ p ->
      checkPattern p
    PRecord _ _ d mp -> do
      traverse_ checkPattern d
      traverse_ checkPattern mp
    PListCons _ _ p1 p2 -> do
      checkPattern p1
      checkPattern p2
    PListLiteral _ _ ps ->
      traverse_ checkPattern ps
    PTuple _ _ ps ->
      traverse_ checkPattern ps
    POr _ _ p1 p2 -> do
      checkPattern p1
      checkPattern p2
    PAs _ _ p ->
      checkPattern p
    PAtVariable a (Label _ name) ->
      checkDup a name
    PVariable a (Label _ name) ->
      checkDup a name
    PShorthand a (Label _ name) ->
      checkDup a name
    PNamedFold a _ (Label _ name) ->
      checkDup a name
    _ ->
      pure ()

checkDup :: (Monad m) => Metadata -> Name -> StateT (Set Name) (CompilerT Metadata m) ()
checkDup loc name = do
  s <- get
  when (name `elem` s) $ do
    path <- lift (gets compilerCurrentModule)
    tellErrors [ConflictingParameter name (ErrorLocation (principalPath path) loc)]
    throwError PreflightFailure
  modify (Set.insert name)
