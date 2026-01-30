{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Coal.Language (Choice (..), Clause (..), Expression (..), Guard (..), Pattern (..))
import Coal.Language.Module
import Control.Monad.Except (MonadError (throwError), void)
import Control.Monad.State (gets)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary, Name, const2, foldrM, traverse_)

passExpressionFolds :: (Monad m, Monoid a, Data a) => Pass a m (Module a k ()) (Module a k ())
passExpressionFolds = Pass{runPass = pass}

pass :: (Monad m, Monoid a, Data a) => Module a k () -> CompilerT a m (Module a k ())
pass = compileFolds

class FoldContext a e where
  expandFolds :: (Monad m) => Name -> [Label ()] -> e -> CompilerT a m e
  expandMatch :: (Monad m) => e -> CompilerT a m ()

instance (FoldContext a e) => FoldContext a [e] where
  expandFolds name = traverse . expandFolds name
  expandMatch = traverse_ expandMatch

instance (FoldContext a e) => FoldContext a (NonEmpty e) where
  expandFolds name = traverse . expandFolds name
  expandMatch = traverse_ expandMatch

instance (Monoid a, Data a) => FoldContext a (Clause a () ()) where
  expandFolds name _ =
    \case
      EClause _ (PAtVariable loc _) _ -> do
        path <- gets compilerCurrentModule
        tellErrors [FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
        throwError PatternAnomaly
      EClause a p cs ->
        EClause a <$> transformM eliminateAtPatterns p <*> expandFolds name (atLabels p) cs
  expandMatch =
    \case
      EClause _ p cs -> do
        void (checkPatterns p)
        expandMatch cs

checkPatterns :: (Monoid a, Data a, Data k, Monad m) => Pattern a () k -> CompilerT a m (Pattern a () k)
checkPatterns =
  \case
    PAtVariable loc _ -> do
      path <- gets compilerCurrentModule
      tellErrors [FoldPatternInRegularMatch (ErrorLocation (principalPath path) loc)]
      throwError PatternAnomaly
    p ->
      descendM checkPatterns p

instance (Monoid a, Data a) => FoldContext a (Choice Expression a () ()) where
  expandFolds name lls =
    \case
      CPlain a gs e ->
        CPlain a gs <$> expandFolds name lls e
  expandMatch =
    \case
      CPlain _ _ e -> do
        expandMatch e

instance (Monoid a, Data a) => FoldContext a (Expression a () ()) where
  expandFolds = flip . foldrM . updateName
  expandMatch _ = pure ()

updateName :: (Monoid a, Data a, Monad m) => Name -> Label () -> Expression a () () -> m (Expression a () ())
updateName name label =
  pure
    . replace
      (labelName label)
      ( const2
          ( applicationE (varE name) (EVariable mempty label :| [])
          )
      )

eliminateAtPatterns :: (Monad m) => Pattern a () () -> CompilerT a m (Pattern a () ())
eliminateAtPatterns =
  \case
    PNamedFold loc _ _ -> do
      path <- gets compilerCurrentModule
      tellErrors [NamedFoldNotAllowed (ErrorLocation (principalPath path) loc)]
      throwError PatternAnomaly
    PAtVariable a ll ->
      pure (PVariable a ll)
    p ->
      pure p

atLabels :: (Data a, Data t) => Pattern a () t -> [Label t]
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

expandFoldExpr :: (Monad m, Monoid a, Data a) => NonEmpty (Expression a () ()) -> NonEmpty (Clause a () ()) -> CompilerT a m (Expression a () ())
expandFoldExpr args clauses = do
  name <- supplied (freshName "fold")
  let var = name <> ".expr"
  e1 <- traverse (expandFolds name []) clauses
  pure $
    deepFlattenApplications $
      letE
        name
        ( lambda1E
            var
            (matchE (varE var) e1)
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

instance (Monoid a, Data a) => CompileFoldsContext a (Expression a () ()) where
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

instance (Monoid a, Data a) => CompileFoldsContext a (Clause a () ()) where
  compileFolds =
    \case
      EClause a p cs ->
        EClause a p <$> traverse compileFolds cs

instance (Monoid a, Data a) => CompileFoldsContext a (Choice Expression a () ()) where
  compileFolds =
    \case
      CPlain a gs e ->
        CPlain a <$> traverse compileFolds gs <*> compileFolds e

instance (Monoid a, Data a) => CompileFoldsContext a (Guard Expression a () ()) where
  compileFolds =
    \case
      CGuard e ->
        CGuard <$> compileFolds e

instance (Monoid a, Data a) => CompileFoldsContext a (Module a k ()) where
  compileFolds =
    \case
      Module p ns o -> do
        setCompilerCurrentModuleC p
        Module p ns <$> compileFolds o

instance (Monoid a, Data a) => CompileFoldsContext a (FunctionDefinition a ()) where
  compileFolds =
    \case
      FunctionDefinition a u w ps e ->
        FunctionDefinition a u w ps <$> compileFolds e

instance (Monoid a, Data a) => CompileFoldsContext a (ConstantDefinition a ()) where
  compileFolds =
    \case
      ConstantDefinition a u w e ->
        ConstantDefinition a u w <$> compileFolds e

instance (Monoid a, Data a) => CompileFoldsContext a (Definition a k ()) where
  compileFolds =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> compileFolds f <*> traverse compileFolds fs
      DConstant loc name g fs ->
        DConstant loc name <$> compileFolds g <*> traverse compileFolds fs
      DInstance loc name (InstanceDefinition ps t ds) ->
        DInstance loc name . InstanceDefinition ps t <$> compileFolds ds
      o ->
        pure o
