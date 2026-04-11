{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpressionFolds (passExpressionFolds) where

import Coal.AST.Flattening (flattenApplicationsDeep)
import Coal.AST.Shorthand
import Coal.AST.Transform (replace)
import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path
import Control.Monad.Except (MonadError (throwError), void)
import Control.Monad.State (get)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary, Name, const2, traverse_)

passExpressionFolds :: (Monad m, Monoid a, Data a, Data k) => Pass a m (Module a k ()) (Module a k ())
passExpressionFolds = Pass{runPass = pass}

pass :: (Monad m, Monoid a, Data a, Data k) => Module a k () -> CompilerT a m (Module a k ())
pass = compileFolds

class ExpressionFoldTransform a e where
  expandFolds :: (Monad m) => Name -> [Label ()] -> e -> CompilerT a m e
  expandMatch :: (Monad m) => e -> CompilerT a m ()

instance (ExpressionFoldTransform a e) => ExpressionFoldTransform a [e] where
  expandFolds name = traverse . expandFolds name
  expandMatch = traverse_ expandMatch

instance (ExpressionFoldTransform a e) => ExpressionFoldTransform a (NonEmpty e) where
  expandFolds name = traverse . expandFolds name
  expandMatch = traverse_ expandMatch

instance (Monoid a, Data a, Data k) => ExpressionFoldTransform a (Clause a k ()) where
  expandFolds name _ =
    \case
      EClause _ (PAtVariable loc _) _ -> do
        CompilerState{protoOcompilerCurrentPath = path} <- get
        -- path <- gets compilerCurrentModule
        tellErrors [FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
        throwError PatternAnomaly
      EClause{..} ->
        EClause clauseMetadata
          <$> transformM eliminateAtPatterns clausePattern
          <*> expandFolds name (atLabels clausePattern) clauseChoices
  expandMatch EClause{..} = do
    void (checkPatterns clausePattern)
    expandMatch clauseChoices

checkPatterns :: (Monoid a, Data a, Data k, Monad m) => Pattern a k () -> CompilerT a m (Pattern a k ())
checkPatterns =
  \case
    PAtVariable loc _ -> do
      CompilerState{protoOcompilerCurrentPath = path} <- get
      -- path <- gets compilerCurrentModule
      tellErrors [FoldPatternInRegularMatch (ErrorLocation (principalPath path) loc)]
      throwError PatternAnomaly
    p ->
      descendM checkPatterns p

instance (Monoid a, Data a, Data k) => ExpressionFoldTransform a (Choice Expression a k ()) where
  expandFolds name lls =
    \case
      CPlain a gs e ->
        CPlain a gs <$> expandFolds name lls e
  expandMatch =
    \case
      CPlain _ _ e ->
        expandMatch e

instance (Monoid a, Data a, Data k) => ExpressionFoldTransform a (Expression a k ()) where
  expandFolds name lls expr = pure (foldr (updateName name) expr lls)
  expandMatch _ = pure ()

updateName :: (Monoid a, Data a, Data k) => Name -> Label () -> Expression a k () -> Expression a k ()
updateName name label =
  replace
    (labelName label)
    ( const2
        ( EApplication
            mempty
            ()
            (EVariable mempty (Label () name))
            (EVariable mempty label :| [])
        )
    )

eliminateAtPatterns :: (Monad m) => Pattern a k () -> CompilerT a m (Pattern a k ())
eliminateAtPatterns =
  \case
    PNamedFold loc _ _ -> do
      CompilerState{protoOcompilerCurrentPath = path} <- get
      -- path <- gets compilerCurrentModule
      tellErrors [NamedFoldNotAllowed (ErrorLocation (principalPath path) loc)]
      throwError PatternAnomaly
    PAtVariable a ll ->
      pure (PVariable a ll)
    p ->
      pure p

atLabels :: (Data a, Data k, Data t) => Pattern a k t -> [Label t]
atLabels = execWriter . go
 where
  go =
    transformM $
      \case
        p@(PAtVariable _ label) -> do
          tell [label]
          pure p
        p ->
          pure p

expandFoldExpr :: (Monad m, Monoid a, Data a, Data k) => NonEmpty (Expression a k ()) -> NonEmpty (Clause a k ()) -> CompilerT a m (Expression a k ())
expandFoldExpr args clauses = do
  name <- supplied (freshName "fold")
  expr <- traverse (expandFolds name []) clauses
  pure $
    flattenApplicationsDeep $
      letE
        name
        ( lambda1E
            (name <> ".expr")
            (matchE (varE (name <> ".expr")) expr)
        )
        (applicationE (varE name) args)

class CompileFoldsContext a e where
  compileFolds :: (Monad m) => e -> CompilerT a m e

instance (CompileFoldsContext a e) => CompileFoldsContext a [e] where
  compileFolds = traverse compileFolds

instance (CompileFoldsContext a e) => CompileFoldsContext a (NonEmpty e) where
  compileFolds = traverse compileFolds

instance (CompileFoldsContext a e) => CompileFoldsContext a (Dictionary e) where
  compileFolds = traverse compileFolds

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (Module a k ()) where
  compileFolds =
    \case
      Module{..} -> do
        setCurrentPathC protoOmodulePath
        newModuleDefinitions <- compileFolds protoOmoduleDefinitions
        return $
          Module
            { protoOmoduleDefinitions = newModuleDefinitions
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (Definition a k ()) where
  compileFolds =
    \case
      DFunction a name def ->
        DFunction a name <$> compileFolds def
      DLet a name def ->
        DLet a name <$> compileFolds def
      DInstance a def ->
        DInstance a <$> compileFolds def
      o ->
        pure o

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (FunctionDefinition a k ()) where
  compileFolds =
    \case
      FunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- compileFolds protoOfunctionDefinitionExpression
        return $
          FunctionDefinition
            { protoOfunctionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (LetDefinition a k ()) where
  compileFolds =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- compileFolds protoOletDefinitionExpression
        return $
          LetDefinition
            { protoOletDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (InstanceDefinition a k ()) where
  compileFolds =
    \case
      InstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- compileFolds protoOinstanceDefinitionImplementations
        return $
          InstanceDefinition
            { protoOinstanceDefinitionImplementations = newInstanceDefinitionImplementations
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (Expression a k ()) where
  compileFolds = transformM go
   where
    go =
      \case
        EFold _ _ es cs ->
          expandFoldExpr es cs
        e@(EMatch _ _ _ cs) -> do
          expandMatch cs
          pure e
        e ->
          pure e

-- instance (Monoid a, Data a) => CompileFoldsContext a (Clause a () ()) where
--  compileFolds =
--    \case
--      EClause a p cs ->
--        EClause a p <$> traverse compileFolds cs
--
-- instance (Monoid a, Data a) => CompileFoldsContext a (Choice Expression a () ()) where
--  compileFolds =
--    \case
--      CPlain a gs e ->
--        CPlain a <$> traverse compileFolds gs <*> compileFolds e
--
-- instance (Monoid a, Data a) => CompileFoldsContext a (Guard Expression a () ()) where
--  compileFolds =
--    \case
--      CGuard e ->
--        CGuard <$> compileFolds e
--
-- instance (Monoid a, Data a) => CompileFoldsContext a (Module a k ()) where
--  compileFolds =
--    \case
--      Module p ns o -> do
--        setCompilerCurrentModuleC p
--        Module p ns <$> compileFolds o
--
-- instance (Monoid a, Data a) => CompileFoldsContext a (FunctionDefinition a ()) where
--  compileFolds =
--    \case
--      FunctionDefinition a u w ps e ->
--        FunctionDefinition a u w ps <$> compileFolds e
--
-- instance (Monoid a, Data a) => CompileFoldsContext a (ConstantDefinition a ()) where
--  compileFolds =
--    \case
--      ConstantDefinition a u w e ->
--        ConstantDefinition a u w <$> compileFolds e
--
-- instance (Monoid a, Data a) => CompileFoldsContext a (Definition a k ()) where
--  compileFolds =
--    \case
--      DFunction loc name f fs ->
--        DFunction loc name <$> compileFolds f <*> traverse compileFolds fs
--      DConstant loc name g fs ->
--        DConstant loc name <$> compileFolds g <*> traverse compileFolds fs
--      DInstance loc name (InstanceDefinition ps t ds) ->
--        DInstance loc name . InstanceDefinition ps t <$> compileFolds ds
--      o ->
--        pure o
