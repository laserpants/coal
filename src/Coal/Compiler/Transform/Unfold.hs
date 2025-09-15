{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Unfold (
  CompileUnfoldsContext (..),
  UnfoldExpansion (..),
  runUnfoldExpansion,
  evalUnfoldExpansion,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (suppliedName)
import Coal.Compiler.Transform.Expression
import Coal.Language (Expression (..), Primitive (..))
import Coal.Language.Module (ConstantDef (..), Definition (..), FunctionDef (..), Module (..))
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Dictionary, Name)

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

expandCodataSelect :: (Monoid a, MonadReader Name m, MonadState Int m) => Name -> Expression a () -> m (Expression a ())
expandCodataSelect field e = do
  name <- suppliedName
  let var = name <> "$_fields"
  pure $
    letE
      var
      e
      ( applicationE
          (selectE ("$_" <> field) (varE var))
          (ELiteral mempty LUnit :| [])
      )

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

instance (Monoid a, Data a) => CompileUnfoldsContext (FunctionDef a ()) where
  compileUnfolds =
    \case
      FunctionDef a u w ps e ->
        FunctionDef a u w ps <$> compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext (ConstantDef a ()) where
  compileUnfolds =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext (Definition a k ()) where
  compileUnfolds =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> compileUnfolds f <*> traverse compileUnfolds fs
      DConstant loc name g fs ->
        DConstant loc name <$> compileUnfolds g <*> traverse compileUnfolds fs
      -- TODO
      o ->
        pure o
