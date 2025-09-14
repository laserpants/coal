{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- FIXME
module Coal.Compiler.Transform.Fold (
  CompileFoldsContext (..),
  FoldExpansion (..),
  FoldError (..),
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
import Coal.Language.Module (ConstantDef (..), Definition (..), FunctionDef (..), InstanceDef (..), Module (..))
import Control.Monad.Except
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM, transform, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Dictionary, Name, const2, foldrM, traverse_)

data FoldError a
  = FoldPatternOutsideConstructor a
  | FoldPatternInRegularMatch a
  deriving (Show, Eq, Ord, Read)

newtype FoldExpansion a e = FoldExpansion {foldExpansionStack :: ExceptT (FoldError a) (RWS Name () Int) e}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    , MonadError (FoldError a)
    )

evalFoldExpansion :: Name -> Int -> FoldExpansion a e -> Either (FoldError a) e
evalFoldExpansion name s = fst . runFoldExpansion name s

runFoldExpansion :: Name -> Int -> FoldExpansion a e -> (Either (FoldError a) e, Int)
runFoldExpansion r s e = (a, s')
 where
  (a, s', _) = runRWS (runExceptT (foldExpansionStack e)) r s

class FoldContext a e where
  expandFolds :: (MonadError (FoldError a) m) => Name -> [Label ()] -> e -> m e
  expandMatch :: (MonadError (FoldError a) m) => e -> m ()

instance (FoldContext a e) => FoldContext a [e] where
  expandFolds name = traverse . expandFolds name
  expandMatch = traverse_ expandMatch

instance (FoldContext a e) => FoldContext a (NonEmpty e) where
  expandFolds name = traverse . expandFolds name
  expandMatch = traverse_ expandMatch

instance (Monoid a, Data a) => FoldContext a (Clause a ()) where
  expandFolds name _ =
    \case
      EClause _ (PAtVariable loc _) _ ->
        throwError (FoldPatternOutsideConstructor loc)
      EClause a p cs ->
        EClause
          a
          (transform eliminateAtPatterns p)
          <$> expandFolds name (atLabels p) cs
  expandMatch =
    \case
      EClause _ p cs -> do
        void (checkPatterns p)
        expandMatch cs

checkPatterns :: (MonadError (FoldError o) m, Data o, Data k) => Pattern o k -> m (Pattern o k)
checkPatterns =
  \case
    PAtVariable loc _ ->
      throwError (FoldPatternInRegularMatch loc)
    p ->
      descendM checkPatterns p

instance (Monoid a, Data a) => FoldContext a (Choice Expression a ()) where
  expandFolds name lls =
    \case
      CPlain a gs e ->
        CPlain a gs <$> expandFolds name lls e
      CLambda{} ->
        error "TODO"
  expandMatch =
    \case
      CPlain _ _ e -> do
        expandMatch e
      CLambda{} ->
        error "TODO"

instance (Monoid a, Data a) => FoldContext a (Expression a ()) where
  expandFolds = flip . foldrM . updateName
  expandMatch _ = pure ()

updateName :: (Monoid a, Data a, Monad m) => Name -> Label () -> Expression a () -> m (Expression a ())
updateName name label =
  pure
    . replace
      (labelName label)
      ( const2
          ( applicationE (varE name) (EVariable mempty label :| [])
          )
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

expandFoldExpr :: (Monoid a, Data a, MonadState Int m, MonadReader Name m, MonadError (FoldError a) m) => NonEmpty (Expression a ()) -> NonEmpty (Clause a ()) -> m (Expression a ())
expandFoldExpr args clauses = do
  name <- suppliedName
  let var = name <> ".expr"
  e1 <- traverse (expandFolds name []) clauses
  pure $
    transform flattenApplication $
      letE
        name
        ( lambda1E
            var
            (matchE (varE var) e1)
        )
        (applicationE (varE name) args)

class CompileFoldsContext a e where
  compileFolds :: e -> FoldExpansion a e

instance (CompileFoldsContext a e) => CompileFoldsContext a [e] where
  compileFolds = traverse compileFolds

instance (CompileFoldsContext a e) => CompileFoldsContext a (NonEmpty e) where
  compileFolds = traverse compileFolds

instance (CompileFoldsContext a e) => CompileFoldsContext a (Dictionary e) where
  compileFolds = traverse compileFolds

instance (Monoid a, Data a) => CompileFoldsContext a (Expression a ()) where
  compileFolds = transformM go
   where
    go =
      \case
        EFold a t es cs Nothing -> do
          e1 <- expandFoldExpr es cs
          pure (EFold a t es cs (Just e1))
        e@(EMatch _ _ _ cs) -> do
          expandMatch cs
          pure e
        e ->
          pure e

instance (Monoid a, Data a) => CompileFoldsContext a (Module a k ()) where
  compileFolds =
    \case
      Module p ns o ->
        Module p ns <$> compileFolds o

instance (Monoid a, Data a) => CompileFoldsContext a (FunctionDef a ()) where
  compileFolds =
    \case
      FunctionDef a u w ps e ->
        FunctionDef a u w ps <$> compileFolds e

instance (Monoid a, Data a) => CompileFoldsContext a (ConstantDef a ()) where
  compileFolds =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> compileFolds e

instance (Monoid a, Data a) => CompileFoldsContext a (Definition a k ()) where
  compileFolds =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> compileFolds f <*> traverse compileFolds fs
      DConstant loc name g fs ->
        DConstant loc name <$> compileFolds g <*> traverse compileFolds fs
      DInstance loc name (InstanceDef ps t ds) ->
        DInstance loc name . InstanceDef ps t <$> compileFolds ds
      o ->
        pure o
