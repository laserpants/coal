{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpressionUnfolds (passExpressionUnfolds) where

import Coal.AST.Shorthand (applicationE, letE, selectE, varE)
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (Choice (..), Clause (..), Expression (..), Guard (..), Primitive (..))
import Coal.Language.Module
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary, Name)

passExpressionUnfolds :: (Monad m, Monoid a, Data a) => Pass a m (Module a k ()) (Module a k ())
passExpressionUnfolds =
  Pass
    { passName = "ExpressionUnfolds"
    , runPass = pass
    }

pass :: (Monad m, Monoid a, Data a) => Module a k () -> CompilerT a m (Module a k ())
pass = compileUnfolds

expandCodataSelect :: (Monoid a, Monad m) => Name -> Expression a () -> CompilerT a m (Expression a ())
expandCodataSelect field e = do
  name <- supplied (freshName "unfold")
  let var = name <> "$_fields"
  pure $
    letE
      var
      e
      ( applicationE
          (selectE ("$_" <> field) (varE var))
          (ELiteral mempty LUnit :| [])
      )

class CompileUnfoldsContext a e where
  compileUnfolds :: (Monad m) => e -> CompilerT a m e

instance (CompileUnfoldsContext a e) => CompileUnfoldsContext a [e] where
  compileUnfolds = traverse compileUnfolds

instance (CompileUnfoldsContext a e) => CompileUnfoldsContext a (NonEmpty e) where
  compileUnfolds = traverse compileUnfolds

instance (CompileUnfoldsContext a e) => CompileUnfoldsContext a (Dictionary e) where
  compileUnfolds = traverse compileUnfolds

instance (Monoid a, Data a) => CompileUnfoldsContext a (Expression a ()) where
  compileUnfolds = transformM $
    \case
      ECodataSelect a ll@(Label _ name) (Just e) Nothing -> do
        e1 <- expandCodataSelect name e
        pure (ECodataSelect a ll Nothing (Just e1))
      e ->
        pure e

instance (Monoid a, Data a) => CompileUnfoldsContext a (Clause a ()) where
  compileUnfolds =
    \case
      EClause a p cs ->
        EClause a p <$> traverse compileUnfolds cs

instance (Monoid a, Data a) => CompileUnfoldsContext a (Choice Expression a ()) where
  compileUnfolds =
    \case
      CPlain a gs e ->
        CPlain a <$> traverse compileUnfolds gs <*> compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext a (Guard Expression a ()) where
  compileUnfolds =
    \case
      CGuard e ->
        CGuard <$> compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext a (Module a k ()) where
  compileUnfolds =
    \case
      Module p ns o ->
        Module p ns <$> compileUnfolds o

instance (Monoid a, Data a) => CompileUnfoldsContext a (FunctionDef a ()) where
  compileUnfolds =
    \case
      FunctionDef a u w ps e ->
        FunctionDef a u w ps <$> compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext a (ConstantDef a ()) where
  compileUnfolds =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext a (FoldDef a ()) where
  compileUnfolds =
    \case
      FoldDef with cs e ->
        FoldDef with <$> traverse compileUnfolds cs <*> traverse compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext a (UnfoldDef a ()) where
  compileUnfolds =
    \case
      UnfoldDef with ps d e ->
        UnfoldDef with ps <$> traverse compileUnfolds d <*> traverse compileUnfolds e

instance (Monoid a, Data a) => CompileUnfoldsContext a (Definition a k ()) where
  compileUnfolds =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> compileUnfolds f <*> traverse compileUnfolds fs
      DConstant loc name g fs ->
        DConstant loc name <$> compileUnfolds g <*> traverse compileUnfolds fs
      DFold loc name d ->
        DFold loc name <$> compileUnfolds d
      DUnfold loc name d ->
        DUnfold loc name <$> compileUnfolds d
      o ->
        pure o
