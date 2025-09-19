{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Pattern.Desugar (Sugared (..)) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (listenPatterns, tellPatterns)
import Coal.Compiler.Stack
import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..))
import Coal.Language.HasType (HasType (..))
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Definition (Definition (..))
import Coal.Language.Module.Definition.Constant (ConstantDef (..))
import Coal.Language.Module.Definition.Fold (FoldDef (..))
import Coal.Language.Module.Definition.Function (FunctionDef (..))
import Coal.Language.Module.Definition.Unfold (UnfoldDef (..))
import Coal.Language.Pattern (IndexedPattern, Pattern (..))
import Coal.Language.Type (IndexedType)
import Coal.Language.Type.Kind (Kind (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Extra (Name)

class Sugared a s where
  desugarPatterns :: (Monad m) => s -> CompilerT a m s

instance (Data s, Monoid s) => Sugared s (IndexedPattern s) where
  desugarPatterns =
    \case
      p@PVariable{} ->
        pure p
      p@(PAnnotation _ _ PVariable{}) ->
        pure p
      p -> do
        name <- supplied (freshName "v")
        tellPatterns (name, p)
        pure (PVariable mempty (Label (typeOf p) name))

instance (Data s, Monoid s) => Sugared s (Binding Expression s IndexedType) where
  desugarPatterns =
    \case
      BPattern a p e ->
        BPattern a <$> desugarPatterns p <*> desugarPatterns e
      BFunction a name ps e ->
        BFunction a name <$> traverse desugarPatterns ps <*> desugarPatterns e

instance (Data s, Monoid s) => Sugared s (Expression s IndexedType) where
  desugarPatterns = transformM go
   where
    go =
      \case
        ELet a gs e1 -> do
          d1 <- desugarPatterns e1
          (hs, ps) <- listenPatterns (traverse desugarPatterns gs)
          pure (ELet a hs (foldr unrollMatch d1 ps))
        ERecursiveLet a p e1 e2 -> do
          d1 <- desugarPatterns e1
          d2 <- desugarPatterns e2
          (q, ps) <- listenPatterns (desugarPatterns p)
          pure (ERecursiveLet a q d1 (foldr unrollMatch d2 ps))
        ELambda a ps e -> do
          e1 <- desugarPatterns e
          (qs, rs) <- listenPatterns (traverse desugarPatterns ps)
          pure (ELambda a qs (foldr unrollMatch e1 rs))
        e ->
          pure e

unrollMatch :: (Data s, Monoid s) => (Name, Pattern s IndexedType) -> Expression s IndexedType -> Expression s IndexedType
unrollMatch (name, p) e =
  EMatch
    mempty
    (typeOf e)
    (EVariable mempty (Label (typeOf p) name))
    (EClause mempty p (CPlain mempty [] e :| []) :| [])

instance (Data s, Monoid s) => Sugared s (FunctionDef s IndexedType) where
  desugarPatterns =
    \case
      FunctionDef a u w ps e -> do
        e1 <- desugarPatterns e
        (qs, rs) <- listenPatterns (traverse desugarPatterns ps)
        pure (FunctionDef a u w qs (foldr unrollMatch e1 rs))

instance (Data s, Monoid s) => Sugared s (ConstantDef s IndexedType) where
  desugarPatterns =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> desugarPatterns e

instance (Data s, Monoid s) => Sugared s (Definition s Kind IndexedType) where
  desugarPatterns =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> desugarPatterns f <*> traverse desugarPatterns fs
      DConstant loc name g fs ->
        DConstant loc name <$> desugarPatterns g <*> traverse desugarPatterns fs
      DFold loc n (FoldDef with cs e) ->
        DFold loc n . FoldDef with cs <$> traverse desugarPatterns e
      DUnfold loc n (UnfoldDef with ps d e) ->
        DUnfold loc n . UnfoldDef with ps d <$> traverse desugarPatterns e
      d ->
        pure d

instance (Data s, Monoid s) => Sugared s (Module s Kind IndexedType) where
  desugarPatterns =
    \case
      Module p ns ds ->
        Module p ns <$> traverse desugarPatterns ds
