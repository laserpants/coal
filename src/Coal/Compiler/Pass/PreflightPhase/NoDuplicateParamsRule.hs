{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.NoDuplicateParamsRule (
  passNoDuplicateParamsRule,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (BuildUnit, Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad.Except
import Control.Monad.State (StateT, evalStateT, get, gets, modify, put)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, traverse_)

passNoDuplicateParamsRule :: (MonadIO m) => Pass Metadata m [BuildUnit (Module Metadata Kind ())] [BuildUnit (Module Metadata Kind ())]
passNoDuplicateParamsRule = mapPass $ Pass{runPass = traverse (withCurrentModuleC_ detectDuplicateParams)}

class RuleContext e where
  detectDuplicateParams :: (Monad m) => e -> CompilerT Metadata m ()

instance (RuleContext e) => RuleContext [e] where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (RuleContext e) => RuleContext (NonEmpty e) where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (RuleContext e) => RuleContext (Maybe e) where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (Data t) => RuleContext (Module Metadata Kind t) where
  detectDuplicateParams =
    \case
      Module _ _ o ->
        detectDuplicateParams o

instance (Data t) => RuleContext (Definition Metadata k t) where
  detectDuplicateParams =
    \case
      DFunction _ _ f ws -> do
        detectDuplicateParams f
        detectDuplicateParams ws
      DConstant _ _ c ws -> do
        detectDuplicateParams c
        detectDuplicateParams ws
      DInstance _ _ d ->
        detectDuplicateParams d
      DFold _ _ d ->
        detectDuplicateParams d
      DUnfold _ _ d ->
        detectDuplicateParams d
      _ ->
        pure ()

instance RuleContext (FunctionDefinition Metadata t) where
  detectDuplicateParams =
    \case
      FunctionDefinition _ _ _ ps e -> do
        checkPatterns ps
        detectDuplicateParams e

instance RuleContext (ConstantDefinition Metadata t) where
  detectDuplicateParams =
    \case
      ConstantDefinition _ _ _ e ->
        detectDuplicateParams e

instance (RuleContext (d a k t)) => RuleContext (InstanceDefinition d a k t) where
  detectDuplicateParams =
    \case
      InstanceDefinition _ _ entries ->
        detectDuplicateParams entries

instance RuleContext (FoldDefinition Metadata t) where
  detectDuplicateParams =
    \case
      FoldDefinition _ cs _ ->
        detectDuplicateParams cs

instance RuleContext (UnfoldDefinition Metadata t) where
  detectDuplicateParams =
    \case
      UnfoldDefinition _ ps fields _ -> do
        checkPatterns ps
        traverse_ detectDuplicateParams fields

instance RuleContext (Clause Metadata t) where
  detectDuplicateParams =
    \case
      EClause _ p c -> do
        checkPatterns (NonEmpty.singleton p)
        detectDuplicateParams c

instance RuleContext (Choice Expression Metadata t) where
  detectDuplicateParams =
    \case
      CPlain _ gs e -> do
        detectDuplicateParams gs
        detectDuplicateParams e

instance RuleContext (Guard Expression Metadata t) where
  detectDuplicateParams =
    \case
      CGuard e ->
        detectDuplicateParams e

instance RuleContext (Expression Metadata t) where
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
      ELambdaMatch _ _ cs me -> do
        detectDuplicateParams cs
        detectDuplicateParams me
      EFold _ _ es cs me -> do
        detectDuplicateParams es
        detectDuplicateParams cs
        detectDuplicateParams me
      ESelect _ _ e ->
        detectDuplicateParams e
      ECodataSelect _ _ me1 me2 -> do
        detectDuplicateParams me1
        detectDuplicateParams me2
      ECodataRecord _ _ d ->
        traverse_ detectDuplicateParams d
      EFocus _ _ _ e1 e2 -> do
        detectDuplicateParams e1
        detectDuplicateParams e2
      EFFICall _ _ _ es e -> do
        detectDuplicateParams es
        detectDuplicateParams e
      _ ->
        pure ()

instance RuleContext (Binding Expression Metadata t) where
  detectDuplicateParams =
    \case
      BPattern _ p e -> do
        checkPatterns (NonEmpty.singleton p)
        detectDuplicateParams e
      BFunction _ _ ps e -> do
        checkPatterns ps
        detectDuplicateParams e

checkPatterns :: (Monad m) => NonEmpty (Pattern Metadata t) -> CompilerT Metadata m ()
checkPatterns patterns = evalStateT (traverse_ checkPattern patterns) mempty
 where
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
      path <- lift (gets compilerCurrentModule)
      tellErrors [ConflictingParameter name (ErrorLocation (principalPath path) loc)]
      throwError PreflightFailure
    registerName name

{-# INLINE registerName #-}
registerName :: (Monad m) => Name -> StateT (Set Name) (CompilerT Metadata m) ()
registerName = modify . Set.insert
