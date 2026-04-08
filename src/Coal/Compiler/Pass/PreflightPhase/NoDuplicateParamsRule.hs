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
import Coal.Compiler.Build.Unit (BuildUnit (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT, setCurrentPathC)
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.Except
import Control.Monad.State (StateT, evalStateT, get, gets, modify, put)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, traverse_)

passNoDuplicateParamsRule :: (MonadIO m) => Pass Metadata m [BuildUnit (Module Metadata Kind ())] [BuildUnit (ProtoModule Metadata () ())]
passNoDuplicateParamsRule = mapPass $ Pass{runPass = traverse fork}

fork :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata () ())
fork m = do
  let mm = toProtoModule [] m
  lift $ setCurrentPathC (protoOmodulePath mm)
  detectDuplicateParams mm
  return mm

class RuleContext e where
  detectDuplicateParams :: (Monad m) => e -> CompilerT Metadata (ProtoCompilerT m Metadata) ()

instance (RuleContext e) => RuleContext [e] where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (RuleContext e) => RuleContext (NonEmpty e) where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (RuleContext e) => RuleContext (Maybe e) where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (Data t) => RuleContext (ProtoModule Metadata () t) where
  detectDuplicateParams =
    \case
      ProtoModule{..} ->
        detectDuplicateParams protoOmoduleDefinitions

instance (Data t) => RuleContext (ProtoDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      ProtoDFunction _ _ def ->
        detectDuplicateParams def
      ProtoDLet _ _ def ->
        detectDuplicateParams def
      ProtoDInstance _ def ->
        detectDuplicateParams def
      ProtoDFold _ _ def ->
        detectDuplicateParams def
      _ ->
        pure ()

instance RuleContext (ProtoFunctionDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      ProtoFunctionDefinition{..} -> do
        checkPatterns protoOfunctionDefinitionPatterns
        detectDuplicateParams protoOfunctionDefinitionExpression

instance RuleContext (ProtoLetDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      ProtoLetDefinition{..} ->
        detectDuplicateParams protoOletDefinitionExpression

instance (Data t) => RuleContext (ProtoInstanceDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      ProtoInstanceDefinition{..} ->
        detectDuplicateParams protoOinstanceDefinitionImplementations

instance RuleContext (ProtoFoldDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      ProtoFoldDefinition{..} ->
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

checkPatterns :: (Monad m) => NonEmpty (Pattern Metadata () t) -> CompilerT Metadata (ProtoCompilerT m Metadata) ()
checkPatterns patterns = evalStateT (traverse_ checkPattern patterns) mempty
 where
  checkPattern :: (Monad m) => Pattern Metadata () t -> StateT (Set Name) (CompilerT Metadata (ProtoCompilerT m Metadata)) ()
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

  checkDup :: (Monad m) => Metadata -> Name -> StateT (Set Name) (CompilerT Metadata (ProtoCompilerT m Metadata)) ()
  checkDup loc name = do
    s <- get
    when (name `elem` s) $ do
      path <- lift $ lift $ gets protoOcompilerCurrentPath
      tellErrors [ConflictingParameter name (ErrorLocation (principalPath path) loc)]
      throwError PreflightFailure
    registerName name

{-# INLINE registerName #-}
registerName :: (Monad m) => Name -> StateT (Set Name) (CompilerT Metadata m) ()
registerName = modify . Set.insert
