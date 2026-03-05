{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpressionFolds (passExpressionFolds) where

import Coal.AST.Flattening (deepFlattenApplications)
import Coal.AST.Shorthand
import Coal.AST.Transform (replace)
import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Coal.Language.Module
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Control.Monad.Except (MonadError (throwError), void)
import Control.Monad.State (get)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary, Name, const2, traverse_)

passExpressionFolds :: (Monad m, Monoid a, Data a, Data k) => Pass a m (ProtoModule a k ()) (ProtoModule a k ())
passExpressionFolds = Pass{runPass = pass}

pass :: (Monad m, Monoid a, Data a, Data k) => ProtoModule a k () -> CompilerT a (ProtoCompilerT m a) (ProtoModule a k ())
pass = compileFolds

class ExpressionFoldTransform a e where
  expandFolds :: (Monad m) => Name -> [Label ()] -> e -> CompilerT a (ProtoCompilerT m a) e
  expandMatch :: (Monad m) => e -> CompilerT a (ProtoCompilerT m a) ()

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
        ProtoCompilerState{protoOcompilerCurrentPath = path} <- lift get
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

checkPatterns :: (Monoid a, Data a, Data k, Monad m) => Pattern a k () -> CompilerT a (ProtoCompilerT m a) (Pattern a k ())
checkPatterns =
  \case
    PAtVariable loc _ -> do
      ProtoCompilerState{protoOcompilerCurrentPath = path} <- lift get
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

eliminateAtPatterns :: (Monad m) => Pattern a k () -> CompilerT a (ProtoCompilerT m a) (Pattern a k ())
eliminateAtPatterns =
  \case
    PNamedFold loc _ _ -> do
      ProtoCompilerState{protoOcompilerCurrentPath = path} <- lift get
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

expandFoldExpr :: (Monad m, Monoid a, Data a, Data k) => NonEmpty (Expression a k ()) -> NonEmpty (Clause a k ()) -> CompilerT a (ProtoCompilerT m a) (Expression a k ())
expandFoldExpr args clauses = do
  name <- supplied (freshName "fold")
  expr <- traverse (expandFolds name []) clauses
  pure $
    deepFlattenApplications $
      letE
        name
        ( lambda1E
            (name <> ".expr")
            (matchE (varE (name <> ".expr")) expr)
        )
        (applicationE (varE name) args)

class CompileFoldsContext a e where
  compileFolds :: (Monad m) => e -> CompilerT a (ProtoCompilerT m a) e

instance (CompileFoldsContext a e) => CompileFoldsContext a [e] where
  compileFolds = traverse compileFolds

instance (CompileFoldsContext a e) => CompileFoldsContext a (NonEmpty e) where
  compileFolds = traverse compileFolds

instance (CompileFoldsContext a e) => CompileFoldsContext a (Dictionary e) where
  compileFolds = traverse compileFolds

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (ProtoModule a k ()) where
  compileFolds =
    \case
      ProtoModule{..} -> do
        lift $ setCurrentPathC protoOmodulePath
        newModuleDefinitions <- compileFolds protoOmoduleDefinitions
        return $
          ProtoModule
            { protoOmoduleDefinitions = newModuleDefinitions
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (ProtoDefinition a k ()) where
  compileFolds =
    \case
      ProtoDFunction a name def ->
        ProtoDFunction a name <$> compileFolds def
      ProtoDLet a name def ->
        ProtoDLet a name <$> compileFolds def
      ProtoDInstance a def ->
        ProtoDInstance a <$> compileFolds def
      o ->
        pure o

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (ProtoFunctionDefinition a k ()) where
  compileFolds =
    \case
      ProtoFunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- compileFolds protoOfunctionDefinitionExpression
        return $
          ProtoFunctionDefinition
            { protoOfunctionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (ProtoLetDefinition a k ()) where
  compileFolds =
    \case
      ProtoLetDefinition{..} -> do
        newLetDefinitionExpression <- compileFolds protoOletDefinitionExpression
        return $
          ProtoLetDefinition
            { protoOletDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileFoldsContext a (ProtoInstanceDefinition a k ()) where
  compileFolds =
    \case
      ProtoInstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- compileFolds protoOinstanceDefinitionImplementations
        return $
          ProtoInstanceDefinition
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
