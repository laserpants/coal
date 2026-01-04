{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.LambdaMatchExpansion (passLambdaMatchExpansion) where

import Coal.AST.Shorthand (lambdaE, matchE, varE, varP)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (Expression (ELambdaMatch), Kind)
import Coal.Language.Module (ConstantDefinition (..), Definition (..), FunctionDefinition (..), Module (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary)

passLambdaMatchExpansion :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind ()) (Module a Kind ())
passLambdaMatchExpansion = Pass{runPass = expandLambdaMatchExprs}

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
      ELambdaMatch _ _ cs ->
        pure $
          lambdaE
            (varP "$lambda_match" :| [])
            (matchE (varE "$lambda_match") cs)
      e ->
        pure e

instance (Monoid a, Data a) => TransformContext (Module a Kind ()) where
  expandLambdaMatchExprs =
    \case
      Module p ns o ->
        Module p ns <$> expandLambdaMatchExprs o

instance (Monoid a, Data a) => TransformContext (FunctionDefinition a ()) where
  expandLambdaMatchExprs =
    \case
      FunctionDefinition a u w ps e ->
        FunctionDefinition a u w ps <$> expandLambdaMatchExprs e

instance (Monoid a, Data a) => TransformContext (ConstantDefinition a ()) where
  expandLambdaMatchExprs =
    \case
      ConstantDefinition a u w e ->
        ConstantDefinition a u w <$> expandLambdaMatchExprs e

instance (Monoid a, Data a) => TransformContext (Definition a Kind ()) where
  expandLambdaMatchExprs =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> expandLambdaMatchExprs f <*> traverse expandLambdaMatchExprs fs
      DConstant loc name g fs ->
        DConstant loc name <$> expandLambdaMatchExprs g <*> traverse expandLambdaMatchExprs fs
      o ->
        pure o
