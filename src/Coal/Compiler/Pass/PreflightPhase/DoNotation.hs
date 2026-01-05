{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.DoNotation (passDoNotation) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Pass (BuildUnit (..), Pass (..), mapPass)
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

passDoNotation :: (MonadIO m) => Pass a m [BuildUnit (Module Metadata Kind ())] [BuildUnit (Module Metadata Kind ())]
passDoNotation = mapPass $ Pass{runPass = traverse desugarDoNotation}

class TransformContext e where
  desugarDoNotation :: (Monad m) => e -> CompilerT a m e

instance (TransformContext a) => TransformContext (Maybe a) where
  desugarDoNotation = traverse desugarDoNotation

instance (Data a, Monoid a) => TransformContext (Module a Kind ()) where
  desugarDoNotation =
    \case
      Module p ns o -> do
        Module p ns <$> traverse desugarDoNotation o

instance (Data a, Monoid a) => TransformContext (Definition a k ()) where
  desugarDoNotation =
    \case
      DFunction a n fs ws -> do
        DFunction a n
          <$> traverse desugarDoNotation fs
          <*> traverse desugarDoNotation ws
      DConstant a n c ws -> do
        DConstant a n
          <$> desugarDoNotation c
          <*> traverse desugarDoNotation ws
      DInstance a n d -> do
        DInstance a n <$> desugarDoNotation d
      DFold a n d -> do
        DFold a n <$> desugarDoNotation d
      DUnfold a n d -> do
        DUnfold a n <$> desugarDoNotation d
      o ->
        pure o

instance (TransformContext (d a k ())) => TransformContext (InstanceDefinition d a k ()) where
  desugarDoNotation =
    \case
      InstanceDefinition ps t es ->
        InstanceDefinition ps t
          <$> traverse desugarDoNotation es

instance (Data a, Monoid a) => TransformContext (FoldDefinition a ()) where
  desugarDoNotation =
    \case
      FoldDefinition a cs ->
        FoldDefinition a
          <$> traverse desugarDoNotation cs

instance (Data a, Monoid a) => TransformContext (UnfoldDefinition a ()) where
  desugarDoNotation =
    \case
      UnfoldDefinition a ps fs e ->
        UnfoldDefinition a ps
          <$> traverse desugarDoNotation fs
          <*> desugarDoNotation e

instance (Data a, Monoid a) => TransformContext (FunctionDefinition a ()) where
  desugarDoNotation =
    \case
      FunctionDefinition a u w ps e ->
        FunctionDefinition a u w ps <$> desugarDoNotation e

instance (Data a, Monoid a) => TransformContext (ConstantDefinition a ()) where
  desugarDoNotation =
    \case
      ConstantDefinition a u w e ->
        ConstantDefinition a u w <$> desugarDoNotation e

instance (Data a, Monoid a) => TransformContext (Expression a ()) where
  desugarDoNotation =
    \case
      EDoBlock _ es ->
        pure $ foldr go e' es'
       where
        (e', es') = normalize es
        bind e1 e2 = EApplication mempty () (EVariable mempty (Label () "bind")) (e1 :| [e2])
        go (p, e) e2 = bind e (ELambda mempty (p :| []) e2)
      e ->
        descendM desugarDoNotation e

normalize :: (Monoid a) => NonEmpty (Pattern a (), Expression a ()) -> (Expression a (), NonEmpty (Pattern a (), Expression a ()))
normalize es =
  case lst of
    (PAny _ (), e) ->
      (e, NonEmpty.fromList $ NonEmpty.init es)
    _ ->
      (EApplication mempty () (EVariable mempty (Label () "pure")) (ELiteral mempty LUnit :| []), es)
 where
  lst = NonEmpty.last es

instance (Data a, Monoid a) => TransformContext (Clause a ()) where
  desugarDoNotation =
    \case
      EClause a p cs ->
        EClause a p <$> traverse desugarDoNotation cs

instance (Data a, Monoid a) => TransformContext (Choice Expression a ()) where
  desugarDoNotation =
    \case
      CPlain a gs e ->
        CPlain a <$> traverse desugarDoNotation gs <*> desugarDoNotation e

instance (Data a, Monoid a) => TransformContext (Guard Expression a ()) where
  desugarDoNotation =
    \case
      CGuard e ->
        CGuard <$> desugarDoNotation e
