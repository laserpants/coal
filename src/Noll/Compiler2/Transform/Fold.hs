{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.Transform.Fold (
  CompileFoldsContext (..),
  FoldExpansion (..),
  runFoldExpansion,
  evalFoldExpansion,
  expandFoldExpr,
) where

import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Lang.Common.List1 (List1, NonEmpty (..))
import Lang.Common.Supply (suppliedName)
import Lang.Label (Label (..), labelName)
import Lang.Utils (Dictionary, Name, const2)
import Noll.Compiler2.Expression
import Noll.Compiler2.Transform (flattenApplication)
import Noll.Compiler2.Transform.Tree (replace)
import Noll.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))

newtype FoldExpansion a = FoldExpansion {foldExpansionStack :: RWS Name () Int a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    )

evalFoldExpansion :: Name -> Int -> FoldExpansion a -> a
evalFoldExpansion name s = fst . runFoldExpansion name s

runFoldExpansion :: Name -> Int -> FoldExpansion a -> (a, Int)
runFoldExpansion r s e = (a, s')
 where
  (a, s', _) = runRWS (foldExpansionStack e) r s

class FoldContext e where
  expandFolds :: Name -> [Label ()] -> e -> e

instance (FoldContext e) => FoldContext [e] where
  expandFolds name = fmap . expandFolds name

instance (FoldContext e) => FoldContext (NonEmpty e) where
  expandFolds name = fmap . expandFolds name

instance (Monoid a, Data a) => FoldContext (Clause a ()) where
  expandFolds name _ =
    \case
      EClause a p cs ->
        EClause
          a
          (transform eliminateAtPatterns p)
          (expandFolds name (atLabels p) cs)

instance (Monoid a, Data a) => FoldContext (Choice Expression a ()) where
  expandFolds name lls =
    \case
      CPlain a gs e ->
        CPlain a gs (expandFolds name lls e)
      CLambda{} ->
        error "TODO"

instance (Monoid a, Data a) => FoldContext (Expression a ()) where
  expandFolds = flip . foldr . updateName

updateName :: (Monoid a, Data a) => Name -> Label () -> Expression a () -> Expression a ()
updateName name label =
  replace (labelName label) $
    const2 $
      applicationE (varE name) (EVariable mempty label :| [])

eliminateAtPatterns :: Pattern a () -> Pattern a ()
eliminateAtPatterns =
  \case
    PAtVariable a ll ->
      PVariable a ll
    p ->
      p

atLabels :: (Data a, Data t) => Pattern a t -> [Label t]
atLabels = execWriter . transformM go
 where
  go =
    \case
      p@(PAtVariable _ label) -> do
        tell [label]
        pure p
      p ->
        pure p

expandFoldExpr :: (Monoid a, Data a, MonadState Int m, MonadReader Name m) => List1 (Expression a ()) -> List1 (Clause a ()) -> m (Expression a ())
expandFoldExpr args clauses = do
  name <- suppliedName
  let var = name <> ".expr"
  pure $
    transform flattenApplication $
      letE
        name
        ( lambda1E
            var
            ( matchE
                (varE var)
                (expandFolds name [] <$> clauses)
            )
        )
        (applicationE (varE name) args)

class CompileFoldsContext a where
  compileFolds :: a -> FoldExpansion a

instance (CompileFoldsContext a) => CompileFoldsContext [a] where
  compileFolds = traverse compileFolds

instance (CompileFoldsContext a) => CompileFoldsContext (NonEmpty a) where
  compileFolds = traverse compileFolds

instance (CompileFoldsContext a) => CompileFoldsContext (Dictionary a) where
  compileFolds = traverse compileFolds

instance (Monoid a, Data a) => CompileFoldsContext (Expression a ()) where
  compileFolds = transformM go
   where
    go =
      \case
        EFold a t es cs Nothing -> do
          e1 <- expandFoldExpr es cs
          pure (EFold a t es cs (Just e1))
        e ->
          pure e

instance (Monoid a, Data a) => CompileFoldsContext (Module a k ()) where
  compileFolds =
    \case
      Module p ns o ->
        Module p ns <$> compileFolds o

instance (Monoid a, Data a) => CompileFoldsContext (Function Expression a ()) where
  compileFolds =
    \case
      Function a u ps e ->
        Function a u ps <$> compileFolds e

instance (Monoid a, Data a) => CompileFoldsContext (Constant Expression a ()) where
  compileFolds =
    \case
      Constant a u e ->
        Constant a u <$> compileFolds e

instance (Monoid a, Data a) => CompileFoldsContext (Definition a k ()) where
  compileFolds =
    \case
      DAnnotation u o -> do
        DAnnotation u <$> compileFolds o
      DFunction name f -> do
        DFunction name <$> compileFolds f
      DConstant name g -> do
        DConstant name <$> compileFolds g
      -- TODO
      o ->
        pure o
