{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Definition.Fold where

import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (suppliedName)
import Coal.Compiler.Transform.Expression
import Coal.Compiler.Transform.Fold (FoldError (..))
import Coal.Compiler.Transform.Tree (replace)
import Coal.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Coal.Language.Module (Definition (..), FoldDef (..))
import Control.Monad.Error
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Name, const2, foldrM)

class TopLevelFoldContext e where
  expandFolds :: (MonadError FoldError m) => Name -> [(Name, Label ())] -> e -> m e

instance (TopLevelFoldContext e) => TopLevelFoldContext [e] where
  expandFolds name = traverse . expandFolds name

instance (TopLevelFoldContext e) => TopLevelFoldContext (NonEmpty e) where
  expandFolds name = traverse . expandFolds name

instance (Monoid a, Data a) => TopLevelFoldContext (Clause a ()) where
  expandFolds name _ =
    \case
      EClause _ PAtVariable{} _ ->
        throwError FoldError
      EClause _ PNamedFold{} _ ->
        throwError FoldError
      EClause a p cs ->
        EClause
          a
          (transform eliminateAtPatterns p)
          <$> expandFolds name (atLabels p) cs

instance (Monoid a, Data a) => TopLevelFoldContext (Choice Expression a ()) where
  expandFolds name lls =
    \case
      CPlain a gs e ->
        CPlain a gs <$> expandFolds name lls e
      CLambda{} ->
        error "Not implemented"

instance (Monoid a, Data a) => TopLevelFoldContext (Expression a ()) where
  expandFolds = flip . foldrM . updateName

updateName :: (Monoid a, Data a, Monad m) => Name -> (Name, Label ()) -> Expression a () -> m (Expression a ())
updateName _ (name, label) =
  pure
    . replace
      (labelName label)
      ( const2 $
          applicationE (varE ("!" <> name)) (EVariable mempty label :| [])
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

compileTopLevelFolds :: (Data a, Monoid a, MonadState Int m, MonadReader Name m, MonadError FoldError m) => Definition a k () -> m (Definition a k ())
compileTopLevelFolds =
  \case
    DFold loc name (FoldDef with cs _) -> do
      e1 <- expandTopLevelFold name cs
      pure $ DFold loc name (FoldDef with cs (Just e1))
    o ->
      pure o

expandTopLevelFold :: (Data a, Monoid a, MonadState Int m, MonadReader Name m, MonadError FoldError m) => Name -> NonEmpty (Clause a ()) -> m (Expression a ())
expandTopLevelFold name clauses = do
  name <- suppliedName
  let var = name <> ".expr"
  e1 <- traverse (expandFolds name []) clauses
  pure $
    lambda1E
      var
      (matchE (varE var) e1)
