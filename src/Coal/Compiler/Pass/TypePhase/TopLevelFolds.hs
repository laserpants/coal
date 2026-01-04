{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.TopLevelFolds (passTopLevelFolds) where

import Coal.AST.Shorthand (applicationE, lambda1E, matchE, varE)
import Coal.AST.Transform (replace)
import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Choice (..), Clause (..), Expression (..), Kind (..), Pattern (..))
import Coal.Language.Module
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (gets)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, foldrM)

passTopLevelFolds :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind ()) (Module a Kind ())
passTopLevelFolds = Pass{runPass = pass}

pass :: (Monad m, Monoid a, Data a) => Module a Kind () -> CompilerT a m (Module a Kind ())
pass = withCurrentModuleC (overModuleDefinitionsM (traverse compileTopLevelFolds))

class TopLevelFoldContext a e where
  expandFolds :: (Monad m) => Name -> [(Name, Label ())] -> e -> CompilerT a m e

instance (TopLevelFoldContext a e) => TopLevelFoldContext a [e] where
  expandFolds name = traverse . expandFolds name

instance (TopLevelFoldContext a e) => TopLevelFoldContext a (NonEmpty e) where
  expandFolds name = traverse . expandFolds name

instance (Monoid a, Data a) => TopLevelFoldContext a (Clause a ()) where
  expandFolds name _ =
    \case
      EClause _ (PAtVariable loc _) _ -> do
        path <- gets compilerCurrentModule
        tellErrors [FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
        throwError PatternAnomaly
      EClause _ (PNamedFold loc _ _) _ -> do
        path <- gets compilerCurrentModule
        tellErrors [FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
        throwError PatternAnomaly
      EClause a p cs ->
        EClause
          a
          (transform eliminateAtPatterns p)
          <$> expandFolds name (atLabels p) cs

instance (Monoid a, Data a) => TopLevelFoldContext a (Choice Expression a ()) where
  expandFolds name lls =
    \case
      CPlain a gs e ->
        CPlain a gs <$> expandFolds name lls e

instance (Monoid a, Data a) => TopLevelFoldContext a (Expression a ()) where
  expandFolds = flip . foldrM . updateName

updateName :: (Monad m, Monoid a, Data a) => Name -> (Name, Label ()) -> Expression a () -> CompilerT a m (Expression a ())
updateName _ (name, label) =
  pure
    . replace
      (labelName label)
      ( \loc _ ->
          applicationE
            (EVariable loc (Label () ("!" <> name)))
            (EVariable loc label :| [])
      )

eliminateAtPatterns :: Pattern a () -> Pattern a ()
eliminateAtPatterns =
  \case
    PNamedFold a _ ll ->
      PVariable a ll
    PAtVariable a ll ->
      PVariable a ll
    p ->
      p

atLabels :: (Data a, Data t) => Pattern a t -> [(Name, Label t)]
atLabels = execWriter . transformM go
 where
  go =
    \case
      p@(PNamedFold _ name label) -> do
        tell [(name, label)]
        pure p
      p ->
        pure p

compileTopLevelFolds :: (Monad m, Monoid a, Data a) => Definition a k () -> CompilerT a m (Definition a k ())
compileTopLevelFolds =
  \case
    DFold loc name (FoldDefinition with cs _) -> do
      e1 <- expandTopLevelFold cs
      pure $ DFold loc name (FoldDefinition with cs (Just e1))
    o ->
      pure o

expandTopLevelFold :: (Monad m, Monoid a, Data a) => NonEmpty (Clause a ()) -> CompilerT a m (Expression a ())
expandTopLevelFold clauses = do
  name <- supplied (freshName "fold")
  let var = name <> ".expr"
  e1 <- traverse (expandFolds name []) clauses
  pure $
    lambda1E
      var
      (matchE (varE var) e1)
