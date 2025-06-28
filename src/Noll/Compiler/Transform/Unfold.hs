{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Unfold (
  compileUnfolds,
  runUnfoldExpansion,
  expandUnfoldExpr,
) where

import Control.Monad.RWS (RWS, evalRWS)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, State, evalState)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Lang.Common.List1 (List1, NonEmpty (..))
import Lang.Common.Supply (suppliedName)
import Lang.Label (Label (..), labelName)
import Lang.Utils (Dictionary, Name, const2)
import Noll.Compiler.Transform (flattenApplication)
import Noll.Compiler.Transform.Tree (replace)
import Noll.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))

import qualified Data.Map.Strict as Map

newtype UnfoldExpansion a = UnfoldExpansion {unfoldExpansionStack :: RWS Name () Int a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    )

runUnfoldExpansion :: Name -> Int -> UnfoldExpansion a -> a
runUnfoldExpansion r s e = fst (evalRWS (unfoldExpansionStack e) r s)

renameRecursiveCall :: (Monoid a, Data a) => Name -> Name -> Expression a () -> Expression a ()
renameRecursiveCall old new = replace old (const2 $ EVariable mempty (Label mempty new))

toLambda :: (Monoid a, Data a) => Expression a () -> Expression a ()
toLambda = ELambda mempty (PAny mempty () :| [])

expandUnfoldExpr :: forall m a d. (Monoid a, Data a, MonadState Int m, MonadReader Name m) => Name -> List1 (Pattern a ()) -> Dictionary (Expression a ()) -> m (Expression a ())
expandUnfoldExpr var ps d = do
  name <- suppliedName
  pure $
    transform flattenApplication $
      ERecursiveLet
        mempty
        (PVariable mempty (Label () name))
        ( ELambda
            mempty
            ps
            ( ECodataFields
                mempty
                ()
                (Map.mapKeys ("$$" <>) (Map.map (toLambda . renameRecursiveCall var name) d))
            )
        )
        (EVariable mempty (Label mempty name))

expandCodataSelect :: forall m a. (Monoid a, Data a, MonadState Int m, MonadReader Name m) => Name -> Expression a () -> m (Expression a ())
expandCodataSelect field e =
  pure $
    EApplication
      mempty
      ()
      (EVariable mempty (Label () ("$$force_" <> field)))
      (e :| [])

class CompileUnfoldsContext a where
  compileUnfolds :: a -> UnfoldExpansion a

instance (CompileUnfoldsContext a) => CompileUnfoldsContext [a] where
  compileUnfolds = traverse compileUnfolds

instance (CompileUnfoldsContext a) => CompileUnfoldsContext (NonEmpty a) where
  compileUnfolds = traverse compileUnfolds

instance (CompileUnfoldsContext a) => CompileUnfoldsContext (Dictionary a) where
  compileUnfolds = traverse compileUnfolds

instance (Monoid a, Data a) => CompileUnfoldsContext (Expression a ()) where
  compileUnfolds = transformM go
   where
    go =
      \case
        EUnfold a t ll name ps d Nothing -> do
          e1 <- expandUnfoldExpr name ps d
          pure (EUnfold a t ll name ps d (Just e1))
        ECodataSelect a ll@(Label _ name) e Nothing -> do
          e1 <- expandCodataSelect name e
          pure (ECodataSelect a ll e (Just e1))
        e ->
          pure e

instance (Monoid a, Data a) => CompileUnfoldsContext (Module a k ()) where
  compileUnfolds =
    \case
      Module p ns o ->
        Module p ns <$> compileUnfolds o

instance (Monoid a, Data a) => CompileUnfoldsContext (Function Expression a ()) where
  compileUnfolds =
    \case
      Function a u ps e ->
        Function a u ps <$> compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext (Constant Expression a ()) where
  compileUnfolds =
    \case
      Constant a u e ->
        Constant a u <$> compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext (Definition a k ()) where
  compileUnfolds =
    \case
      DAnnotation u o -> do
        DAnnotation u <$> compileUnfolds o
      DFunction name f -> do
        DFunction name <$> compileUnfolds f
      DConstant name g -> do
        DConstant name <$> compileUnfolds g
      -- TODO
      o ->
        pure o
