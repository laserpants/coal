{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.TopLevelFolds (passTopLevelFolds) where

import Coal.AST.Shorthand (applicationE, lambda1E, matchE, varE)
import Coal.AST.Transform (replace)
import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Choice (..), Clause (..), Expression (..), Kind (..), Pattern (..), With (..))
import Coal.Language.Module.Path (principalPath)
import Coal.ProtoCompiler.ProtoJournal
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (get, gets)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, foldrM)

passTopLevelFolds :: (Monad m, Monoid a, Data a) => Pass a m (ProtoModule a Kind ()) (ProtoModule a Kind ())
passTopLevelFolds = Pass{runPass = pass}

pass :: (Monad m, Monoid a, Data a) => ProtoModule a Kind () -> CompilerT a (ProtoCompilerT m a) (ProtoModule a Kind ())
pass ProtoModule{..} = do
  lift $ setCurrentPathC protoOmodulePath
  -- withCurrentModuleC (overModuleDefinitionsM (traverse compileTopLevelFolds))
  newModuleDefinitions <- traverse compileTopLevelFolds protoOmoduleDefinitions
  return $
    ProtoModule
      { protoOmoduleDefinitions = newModuleDefinitions
      , ..
      }

compileTopLevelFolds :: (Monad m, Monoid a, Data a) => ProtoDefinition a Kind () -> CompilerT a (ProtoCompilerT m a) (ProtoDefinition a Kind ())
compileTopLevelFolds =
  \case
    ProtoDFold loc name ProtoFoldDefinition{..} -> do
      newExpression <- expandClauses protoOfoldDefinitionClauses
      let def =
            ProtoLetDefinition
              { protoOletDefinitionMetadata = loc
              , protoOletDefinitionAnnotation = protoOfoldDefinitionAnnotation
              , protoOletDefinitionType = With [] ()
              , protoOletDefinitionExpression = newExpression
              }
      return (ProtoDLet loc name def)
    o ->
      return o

expandClauses :: (Monad m, Monoid a, Data a) => NonEmpty (Clause a Kind ()) -> CompilerT a (ProtoCompilerT m a) (Expression a Kind ())
expandClauses clauses = do
  name <- lift $ supplied (freshName "fold")
  expr <- traverse (expandFolds name []) clauses
  pure $ lambda1E (name <> ".expr") (matchE (varE (name <> ".expr")) expr)

class TopLevelFoldContext a e where
  expandFolds :: (Monad m) => Name -> [(Name, Label ())] -> e -> CompilerT a (ProtoCompilerT m a) e

instance (TopLevelFoldContext a e) => TopLevelFoldContext a [e] where
  expandFolds name = traverse . expandFolds name

instance (TopLevelFoldContext a e) => TopLevelFoldContext a (NonEmpty e) where
  expandFolds name = traverse . expandFolds name

instance (Monoid a, Data a) => TopLevelFoldContext a (Clause a Kind ()) where
  expandFolds name _ =
    \case
      EClause _ (PAtVariable loc _) _ -> do
        ProtoCompilerState{protoOcompilerCurrentPath = path} <- lift get
        lift $ tellErrors [ProtoError] -- FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
        throwError PatternAnomaly
      EClause _ (PNamedFold loc _ _) _ -> do
        ProtoCompilerState{protoOcompilerCurrentPath = path} <- lift get
        lift $ tellErrors [ProtoError] -- FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
        throwError PatternAnomaly
      EClause{..} -> do
        newClauseChoices <- expandFolds name (atLabels clausePattern) clauseChoices
        return $
          EClause
            clauseMetadata
            (transform eliminateAtPatterns clausePattern)
            newClauseChoices

--       path <- gets compilerCurrentModule
--       tellErrors [FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
--       throwError PatternAnomaly
--     EClause _ (PNamedFold loc _ _) _ -> do
--       path <- gets compilerCurrentModule
--       tellErrors [FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
--       throwError PatternAnomaly
--     EClause a p cs ->
--       EClause
--         a
--         (transform eliminateAtPatterns p)
--         <$> expandFolds name (atLabels p) cs

instance (Monoid a, Data a) => TopLevelFoldContext a (Choice Expression a Kind ()) where
  expandFolds name lls =
    \case
      CPlain a gs e ->
        CPlain a gs <$> expandFolds name lls e

instance (Monoid a, Data a) => TopLevelFoldContext a (Expression a Kind ()) where
  expandFolds = flip . foldrM . const (uncurry updateName)

updateName :: (Monad m, Monoid a, Data a) => Name -> Label () -> Expression a Kind () -> CompilerT a (ProtoCompilerT m a) (Expression a Kind ())
updateName name label =
  pure
    . replace
      (labelName label)
      ( \loc _ ->
          applicationE
            --            (EVariable loc (Label () ("!" <> name)))
            (EVariable loc (Label () name))
            (EVariable loc label :| [])
      )

eliminateAtPatterns :: Pattern a Kind () -> Pattern a Kind ()
eliminateAtPatterns =
  \case
    PNamedFold a _ ll ->
      PVariable a ll
    PAtVariable a ll ->
      PVariable a ll
    p ->
      p

atLabels :: (Data a, Data t) => Pattern a Kind t -> [(Name, Label t)]
atLabels = execWriter . transformM go
 where
  go =
    \case
      p@(PNamedFold _ name label) -> do
        tell [(name, label)]
        pure p
      p ->
        pure p

-- updateName :: (Monad m, Monoid a, Data a) => Name -> (Name, Label ()) -> Expression a () () -> CompilerT a m (Expression a () ())
-- updateName _ (name, label) =
--  pure
--    . replace
--      (labelName label)
--      ( \loc _ ->
--          applicationE
--            (EVariable loc (Label () ("!" <> name)))
--            (EVariable loc label :| [])
--      )
--
-- eliminateAtPatterns :: Pattern a () () -> Pattern a () ()
-- eliminateAtPatterns =
--  \case
--    PNamedFold a _ ll ->
--      PVariable a ll
--    PAtVariable a ll ->
--      PVariable a ll
--    p ->
--      p
--
-- atLabels :: (Data a, Data t) => Pattern a () t -> [(Name, Label t)]
-- atLabels = execWriter . transformM go
-- where
--  go =
--    \case
--      p@(PNamedFold _ name label) -> do
--        tell [(name, label)]
--        pure p
--      p ->
--        pure p
--
-- compileTopLevelFolds :: (Monad m, Monoid a, Data a) => Definition a k () -> CompilerT a m (Definition a k ())
-- compileTopLevelFolds =
--  \case
--    DFold loc name (FoldDefinition w cs) -> do
--      e1 <- expandTopLevelFold cs
--      pure (DConstant loc name (ConstantDefinition loc w (With [] ()) e1) [])
--    o ->
--      pure o
--
-- expandTopLevelFold :: (Monad m, Monoid a, Data a) => NonEmpty (Clause a () ()) -> CompilerT a m (Expression a () ())
-- expandTopLevelFold clauses = do
--  name <- supplied (freshName "fold")
--  let var = name <> ".expr"
--  e1 <- traverse (expandFolds name []) clauses
--  pure $
--    lambda1E
--      var
--      (matchE (varE var) e1)
