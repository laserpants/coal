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
  expandUnfoldExpr,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (suppliedName)
import Coal.Compiler.Transform.Expression
import Coal.Compiler.Transform.Flattening (flattenApplication)
import Coal.Compiler.Transform.Tree (replace)
import Coal.Language (Expression (..), Pattern (..), Primitive (..))
import Coal.Language.Module (Constant (..), Definition (..), Function (..), Module (..))
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Dictionary, Name, const2)

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

expandUnfoldExpr :: (Monoid a, Data a, MonadState Int m, MonadReader Name m) => Name -> NonEmpty (Pattern a ()) -> Dictionary (Expression a ()) -> m (Expression a ())
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
                (Map.mapKeys ("$_" <>) (Map.map (lambdaAnyE . renameRecursiveCall var name) d))
            )
        )
        (varE name)

expandCodataSelect :: (Monoid a, MonadState Int m) => Name -> Expression a () -> m (Expression a ())
expandCodataSelect field e =
  pure $
    letE
      "$_fields"
      e
      ( applicationE
          (selectE ("$_" <> field) (varE "$_fields"))
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
        EUnfold a t name ps d Nothing -> do
          e1 <- expandUnfoldExpr name ps d
          pure (EUnfold a t name ps d (Just e1))
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
      Function a u w ps e ->
        Function a u w ps <$> compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext (Constant Expression a ()) where
  compileUnfolds =
    \case
      Constant a u w e ->
        Constant a u w <$> compileUnfolds e

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
