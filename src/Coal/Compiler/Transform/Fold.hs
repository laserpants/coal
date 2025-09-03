{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Fold (
  CompileFoldsContext (..),
  FoldExpansion (..),
  runFoldExpansion,
  evalFoldExpansion,
  expandFoldExpr,
) where

import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (suppliedName)
import Coal.Compiler.Transform.Expression
import Coal.Compiler.Transform.Flattening (flattenApplication)
import Coal.Compiler.Transform.Tree (replace)
import Coal.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Coal.Language.Module (ConstantDef (..), Definition (..), FunctionDef (..), Module (..))
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Dictionary, Name, const2)

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

expandFoldExpr :: (Monoid a, Data a, MonadState Int m, MonadReader Name m) => NonEmpty (Expression a ()) -> NonEmpty (Clause a ()) -> m (Expression a ())
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

instance (Monoid a, Data a) => CompileFoldsContext (FunctionDef a ()) where
  compileFolds =
    \case
      FunctionDef a u w ps e ->
        FunctionDef a u w ps <$> compileFolds e

instance (Monoid a, Data a) => CompileFoldsContext (ConstantDef a ()) where
  compileFolds =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> compileFolds e

instance (Monoid a, Data a) => CompileFoldsContext (Definition a k ()) where
  compileFolds =
    \case
      DFunction loc name f fs -> do
        DFunction loc name <$> compileFolds f <*> traverse compileFolds fs
      DConstant loc name g fs -> do
        DConstant loc name <$> compileFolds g <*> traverse compileFolds fs
      DInstance loc name ps t ds ->
        DInstance loc name ps t <$> compileFolds ds
      o ->
        pure o
