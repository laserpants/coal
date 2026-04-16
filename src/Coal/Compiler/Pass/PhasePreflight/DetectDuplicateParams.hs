-- +
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhasePreflight.DetectDuplicateParams

Detect duplicate parameter names in function and type definitions.

This pass validates that all parameter names within a single definition are
unique, reporting errors for duplicate parameters in:

- Function definitions: @fun f(x, x) = ...@
- Type constructors: @type Pair<a, a> = Pair(a, a)@
- Lambda expressions: @fn(x, x) => ...@

Duplicate parameters are ambiguous and would lead to confusion about which
parameter is being referenced in the definition body.

For example, this would be detected as an error:

@
fun add(x, x) = x + x  // Which x?
@

The pass ensures parameter names are unique within their scope, preventing
ambiguous references.
-}
module Coal.Compiler.Pass.PhasePreflight.DetectDuplicateParams (
  passDetectDuplicateParams,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Compiler.State (CompilerState (compilerCurrentPath))
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad.Except
import Control.Monad.State (StateT, evalStateT, get, gets, modify, put)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, traverse_)

{- | Duplicate parameter detection pass.

Validate that all parameter names within function definitions, type
constructors, and lambda expressions are unique. Report errors for any
duplicate parameters that would create ambiguous references.
-}
passDetectDuplicateParams :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passDetectDuplicateParams = mapPass $ Pass{runPass = traverse passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
passImpl m = do
  setCurrentModuleC m
  (_, errors) <- listenErrors (detectDuplicateParams m)
  unless (null errors) $
    throwError PreflightFailure
  return m

class DuplicateParamsContext e where
  detectDuplicateParams :: (Monad m) => e -> CompilerT Metadata m ()

instance (DuplicateParamsContext e) => DuplicateParamsContext [e] where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (DuplicateParamsContext e) => DuplicateParamsContext (NonEmpty e) where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (DuplicateParamsContext e) => DuplicateParamsContext (Maybe e) where
  detectDuplicateParams = traverse_ detectDuplicateParams

instance (Data t) => DuplicateParamsContext (Module Metadata () t) where
  detectDuplicateParams =
    \case
      Module{..} ->
        detectDuplicateParams moduleDefinitions

instance (Data t) => DuplicateParamsContext (Definition Metadata () t) where
  detectDuplicateParams =
    \case
      DType loc _ def ->
        checkTypeParameters loc (typeDefinitionParameters def)
      DTypeAlias loc _ def ->
        checkTypeParameters loc (aliasDefinitionParameters def)
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

instance DuplicateParamsContext (FunctionDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      FunctionDefinition{..} -> do
        checkPatterns functionDefinitionPatterns
        detectDuplicateParams functionDefinitionExpression

instance DuplicateParamsContext (LetDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      LetDefinition{..} ->
        detectDuplicateParams letDefinitionExpression

instance (Data t) => DuplicateParamsContext (InstanceDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      InstanceDefinition{..} ->
        detectDuplicateParams instanceDefinitionImplementations

instance DuplicateParamsContext (FoldDefinition Metadata () t) where
  detectDuplicateParams =
    \case
      FoldDefinition{..} ->
        detectDuplicateParams foldDefinitionClauses

instance DuplicateParamsContext (Clause Metadata () t) where
  detectDuplicateParams =
    \case
      EClause{..} -> do
        checkPatterns (NonEmpty.singleton clausePattern)
        detectDuplicateParams clauseChoices

instance DuplicateParamsContext (Choice Expression Metadata () t) where
  detectDuplicateParams =
    \case
      CPlain{..} -> do
        detectDuplicateParams choiceGuards
        detectDuplicateParams choiceExpression

instance DuplicateParamsContext (Guard Expression Metadata () t) where
  detectDuplicateParams =
    \case
      CGuard{..} ->
        detectDuplicateParams guardExpression

instance DuplicateParamsContext (Expression Metadata () t) where
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

instance DuplicateParamsContext (Binding Expression Metadata () t) where
  detectDuplicateParams =
    \case
      BPattern _ p e -> do
        checkPatterns (NonEmpty.singleton p)
        detectDuplicateParams e
      BFunction _ _ ps e -> do
        checkPatterns ps
        detectDuplicateParams e

checkTypeParameters :: (Monad m) => Metadata -> [Parameter ()] -> CompilerT Metadata m ()
checkTypeParameters loc params = evalStateT (traverse_ checkParam params) mempty
 where
  checkParam :: (Monad m) => Parameter () -> StateT (Set Name) (CompilerT Metadata m) ()
  checkParam (Parameter _ name) = do
    s <- get
    when (name `elem` s) $ do
      path <- lift $ gets compilerCurrentPath
      tellErrors [ConflictingParameter name (ErrorLocation (principalPath path) loc)]
    registerName name

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
      path <- lift $ gets compilerCurrentPath
      tellErrors [ConflictingParameter name (ErrorLocation (principalPath path) loc)]
    registerName name

{-# INLINE registerName #-}
registerName :: (Monad m) => Name -> StateT (Set Name) (CompilerT Metadata m) ()
registerName = modify . Set.insert
