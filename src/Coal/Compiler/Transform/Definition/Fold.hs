{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Definition.Fold where

import Coal.Common.Label (Label (..), labelName)
import Coal.Compiler.Transform.Expression
import Coal.Compiler.Transform.Tree (replace)
import Coal.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Coal.Language.Module (Definition (..), FoldDef (..))
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Name, const2)

class TopLevelFoldContext e where
  expandFolds :: Name -> [(Name, Label ())] -> e -> e

instance (TopLevelFoldContext e) => TopLevelFoldContext [e] where
  expandFolds name = fmap . expandFolds name

instance (TopLevelFoldContext e) => TopLevelFoldContext (NonEmpty e) where
  expandFolds name = fmap . expandFolds name

instance (Monoid a, Data a) => TopLevelFoldContext (Clause a ()) where
  expandFolds name _ =
    \case
      EClause a p cs ->
        EClause
          a
          (transform eliminateAtPatterns p)
          (expandFolds name (atLabels p) cs)

instance (Monoid a, Data a) => TopLevelFoldContext (Choice Expression a ()) where
  expandFolds name lls =
    \case
      CPlain a gs e ->
        CPlain a gs (expandFolds name lls e)
      CLambda{} ->
        error "Not implemented"

instance (Monoid a, Data a) => TopLevelFoldContext (Expression a ()) where
  expandFolds = flip . foldr . updateName

updateName :: (Monoid a, Data a) => Name -> (Name, Label ()) -> Expression a () -> Expression a ()
updateName _ (name, label) =
  replace (labelName label) $
    const2 $
      applicationE (varE ("!" <> name)) (EVariable mempty label :| [])

eliminateAtPatterns :: Pattern a () -> Pattern a ()
eliminateAtPatterns =
  \case
    PNamedAtVariable a _ ll ->
      PVariable a ll
    p ->
      p

atLabels :: (Data a, Data t) => Pattern a t -> [(Name, Label t)]
atLabels = execWriter . transformM go
 where
  go =
    \case
      p@(PNamedAtVariable _ name label) -> do
        tell [(name, label)]
        pure p
      p ->
        pure p

compileTopLevelFolds :: (Data a, Monoid a, Monad m) => Definition a k () -> m (Definition a k ())
compileTopLevelFolds =
  \case
    DFold loc name (FoldDef with cs _) -> do
      e1 <- expandTopLevelFold name cs
      pure $ DFold loc name (FoldDef with cs (Just e1))
    o ->
      pure o

expandTopLevelFold :: (Data a, Monoid a, Monad m) => Name -> NonEmpty (Clause a ()) -> m (Expression a ())
expandTopLevelFold name clauses = do
  pure $
    lambda1E
      var
      ( matchE
          (varE var)
          (expandFolds name [] <$> clauses)
      )
 where
  var = "$fold.expr"
