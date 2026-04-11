{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.NoDuplicateParamsRule (
  passNoDuplicateParamsRule,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad.Except
import Control.Monad.State (StateT, evalStateT, get, gets, modify, put)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, traverse_)

passNoDuplicateParamsRule :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passNoDuplicateParamsRule = mapPass $ Pass{runPass = traverse fork}

fork :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
fork mm = do
  --  let mm = toModule [] m
  setCurrentPathC (protoOmodulePath mm)
  detectDuplicateParams mm
  return mm

class RuleContext e where
  detectDuplicateParams :: (Monad m) => e -> CompilerT Metadata m ()

instance (RuleContext e) => RuleContext [e] where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (RuleContext e) => RuleContext (NonEmpty e) where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (RuleContext e) => RuleContext (Maybe e) where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (Data t) => RuleContext (Module Metadata () t) where
  detectDuplicateParams =
    \case
      Module{..} ->
        detectDuplicateParams protoOmoduleDefinitions

instance (Data t) => RuleContext (Definition Metadata () t) where
  detectDuplicateParams =
    \case
      DFunction _ _ def ->
        detectDuplicateParams def
      DLet _ _ def ->
        detectDuplicateParams def
      DInstance _ def ->
        detectDuplicateParams def
      DFold _ _ def ->
        detectDuplicateParams def
      _ ->
        pure ()

instance RuleContext (FunctionDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      FunctionDefinition{..} -> do
        checkPatterns protoOfunctionDefinitionPatterns
        detectDuplicateParams protoOfunctionDefinitionExpression

instance RuleContext (LetDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      LetDefinition{..} ->
        detectDuplicateParams protoOletDefinitionExpression

instance (Data t) => RuleContext (InstanceDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      InstanceDefinition{..} ->
        detectDuplicateParams protoOinstanceDefinitionImplementations

instance RuleContext (FoldDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      FoldDefinition{..} ->
        detectDuplicateParams protoOfoldDefinitionClauses

instance RuleContext (Clause Metadata () t) where
  detectDuplicateParams =
    \case
      EClause{..} -> do
        checkPatterns (NonEmpty.singleton clausePattern)
        detectDuplicateParams clauseChoices

instance RuleContext (Choice Expression Metadata () t) where
  detectDuplicateParams =
    \case
      CPlain{..} -> do
        detectDuplicateParams choiceGuards
        detectDuplicateParams choiceExpression

instance RuleContext (Guard Expression Metadata () t) where
  detectDuplicateParams =
    \case
      CGuard{..} ->
        detectDuplicateParams guardExpression

instance RuleContext (Expression Metadata () t) where
  detectDuplicateParams =
    \case
      EAnnotation _ _ e ->
        detectDuplicateParams e
      EApplication _ _ e es -> do
        detectDuplicateParams e
        detectDuplicateParams es
      ELambda _ ps e -> do
        checkPatterns ps
        detectDuplicateParams e
      ELet _ bs e -> do
        detectDuplicateParams bs
        detectDuplicateParams e
      ERecursiveLet _ p e1 e2 -> do
        checkPatterns (NonEmpty.singleton p)
        detectDuplicateParams e1
        detectDuplicateParams e2
      EIf _ _ e1 e2 e3 -> do
        detectDuplicateParams e1
        detectDuplicateParams e2
        detectDuplicateParams e3
      ERecord _ _ d me -> do
        traverse_ detectDuplicateParams d
        detectDuplicateParams me
      EListCons _ _ e1 e2 -> do
        detectDuplicateParams e1
        detectDuplicateParams e2
      EListLiteral _ _ es ->
        detectDuplicateParams es
      ETuple _ _ es ->
        detectDuplicateParams es
      EMatch _ _ e cs -> do
        detectDuplicateParams e
        detectDuplicateParams cs
      ELambdaMatch _ _ cs ->
        detectDuplicateParams cs
      EFold _ _ es cs -> do
        detectDuplicateParams es
        detectDuplicateParams cs
      ESelect _ _ e ->
        detectDuplicateParams e
      EFocus _ _ _ _ e1 e2 -> do
        detectDuplicateParams e1
        detectDuplicateParams e2
      EFFICall _ _ _ es e -> do
        detectDuplicateParams es
        detectDuplicateParams e
      _ ->
        pure ()

instance RuleContext (Binding Expression Metadata () t) where
  detectDuplicateParams =
    \case
      BPattern _ p e -> do
        checkPatterns (NonEmpty.singleton p)
        detectDuplicateParams e
      BFunction _ _ ps e -> do
        checkPatterns ps
        detectDuplicateParams e

checkPatterns :: (Monad m) => NonEmpty (Pattern Metadata () t) -> CompilerT Metadata m ()
checkPatterns patterns = evalStateT (traverse_ checkPattern patterns) mempty
 where
  checkPattern :: (Monad m) => Pattern Metadata () t -> StateT (Set Name) (CompilerT Metadata m) ()
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
        s <- get
        checkPattern p1
        put s
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
      path <- lift $ gets protoOcompilerCurrentPath
      tellErrors [ConflictingParameter name (ErrorLocation (principalPath path) loc)]
      throwError PreflightFailure
    registerName name

{-# INLINE registerName #-}
registerName :: (Monad m) => Name -> StateT (Set Name) (CompilerT Metadata m) ()
registerName = modify . Set.insert
