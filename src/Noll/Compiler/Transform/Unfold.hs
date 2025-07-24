{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Unfold (
  CompileUnfoldsContext (..),
  UnfoldExpansion (..),
  runUnfoldExpansion,
  evalUnfoldExpansion,
  expandUnfoldExpr,
) where

import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Lang.Common.List1 (List1, NonEmpty (..))
import Lang.Common.Supply (suppliedName)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Name, const2)
import Noll.Compiler.Transform.Expression
import Noll.Compiler.Transform (flattenApplication)
import Noll.Compiler.Transform.Tree (replace)
import Noll.Language (Expression (..), Pattern (..))
import Noll.Language.Module (Constant (..), Definition (..), Function (..), Module (..))

import qualified Data.Map.Strict as Map

newtype UnfoldExpansion a = UnfoldExpansion {unfoldExpansionStack :: RWS Name () Int a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    )

evalUnfoldExpansion :: Name -> Int -> UnfoldExpansion a -> a
evalUnfoldExpansion name s = fst . runUnfoldExpansion name s

runUnfoldExpansion :: Name -> Int -> UnfoldExpansion a -> (a, Int)
runUnfoldExpansion r s e = (a, s')
 where
  (a, s', _) = runRWS (unfoldExpansionStack e) r s

renameRecursiveCall :: (Monoid a, Data a) => Name -> Name -> Expression a () -> Expression a ()
renameRecursiveCall old new = replace old (const2 $ varE new)

expandUnfoldExpr :: (Monoid a, Data a, MonadState Int m, MonadReader Name m) => Name -> List1 (Pattern a ()) -> Dictionary (Expression a ()) -> m (Expression a ())
expandUnfoldExpr var ps d = do
  name <- suppliedName
  pure $
    transform flattenApplication $
      letE
        name
        ( lambdaE
            ps
            ( ECodataFields
                mempty
                ()
                (Map.mapKeys ("$$" <>) (Map.map (lambdaAnyE . renameRecursiveCall var name) d))
            )
        )
        (varE name)

expandCodataSelect :: (Monoid a, MonadState Int m) => Name -> Expression a () -> m (Expression a ())
expandCodataSelect field e =
  pure $ applicationE (varE ("$$force_" <> field)) (e :| [])

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
