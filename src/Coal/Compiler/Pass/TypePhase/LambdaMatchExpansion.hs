{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.LambdaMatchExpansion (passLambdaMatchExpansion) where

import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Compiler.Transform.Expression
import Coal.Language
import Coal.Language.Module (ConstantDef (..), Definition (..), FunctionDef (..), Module (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary)

passLambdaMatchExpansion :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind ()) (Module a Kind ())
passLambdaMatchExpansion =
  Pass
    { passName = "LambdaMatchExpansion"
    , runPass = expandLambdaMatchExprs
    }

class TransformContext c where
  expandLambdaMatchExprs :: (Monad m) => c -> CompilerT a m c

instance (TransformContext a) => TransformContext [a] where
  expandLambdaMatchExprs = traverse expandLambdaMatchExprs

instance (TransformContext a) => TransformContext (NonEmpty a) where
  expandLambdaMatchExprs = traverse expandLambdaMatchExprs

instance (TransformContext a) => TransformContext (Dictionary a) where
  expandLambdaMatchExprs = traverse expandLambdaMatchExprs

instance (Monoid a, Data a) => TransformContext (Expression a ()) where
  expandLambdaMatchExprs = transformM $
    \case
      ELambdaMatch a t cs Nothing -> do
        e1 <- expandLambdaMatch cs
        pure (ELambdaMatch a t cs (Just e1))
      e ->
        pure e

expandLambdaMatch :: (Monoid a, Monad m) => NonEmpty (Clause a ()) -> CompilerT o m (Expression a ())
expandLambdaMatch cs =
  pure $
    lambdaE
      (varP "$lambda_match" :| [])
      (matchE (varE "$lambda_match") cs)

instance (Monoid a, Data a) => TransformContext (Module a Kind ()) where
  expandLambdaMatchExprs =
    \case
      Module p ns o ->
        Module p ns <$> expandLambdaMatchExprs o

instance (Monoid a, Data a) => TransformContext (FunctionDef a ()) where
  expandLambdaMatchExprs =
    \case
      FunctionDef a u w ps e ->
        FunctionDef a u w ps <$> expandLambdaMatchExprs e

instance (Monoid a, Data a) => TransformContext (ConstantDef a ()) where
  expandLambdaMatchExprs =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> expandLambdaMatchExprs e

instance (Monoid a, Data a) => TransformContext (Definition a Kind ()) where
  expandLambdaMatchExprs =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> expandLambdaMatchExprs f <*> traverse expandLambdaMatchExprs fs
      DConstant loc name g fs ->
        DConstant loc name <$> expandLambdaMatchExprs g <*> traverse expandLambdaMatchExprs fs
      o ->
        pure o
