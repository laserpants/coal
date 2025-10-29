{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.ExpandPatterns (passExpandPatterns) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (listenPatterns, tellPatterns1)
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language (IndexedType, Kind (..))
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
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Extras (Name)

passExpandPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passExpandPatterns =
  Pass
    { passName = "ExpandPatterns"
    , runPass = desugarPatterns
    }

class TransformContext s where
  desugarPatterns :: (Monad m) => s -> CompilerT Metadata m s

instance TransformContext (IndexedPattern Metadata) where
  desugarPatterns =
    \case
      p@PVariable{} ->
        pure p
      p@(PAnnotation _ _ PVariable{}) ->
        pure p
      p -> do
        name <- supplied (freshName "v")
        tellPatterns1 (name, p)
        pure (PVariable mempty (Label (typeOf p) name))

instance TransformContext (Binding Expression Metadata IndexedType) where
  desugarPatterns =
    \case
      BPattern a p e ->
        BPattern a <$> desugarPatterns p <*> desugarPatterns e
      BFunction a name ps e ->
        BFunction a name <$> traverse desugarPatterns ps <*> desugarPatterns e

instance TransformContext (Expression Metadata IndexedType) where
  desugarPatterns = go
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
          descendM go e

unrollMatch :: (Name, Pattern Metadata IndexedType) -> Expression Metadata IndexedType -> Expression Metadata IndexedType
unrollMatch (name, p) e =
  EMatch
    mempty
    (typeOf e)
    (EVariable mempty (Label (typeOf p) name))
    (EClause mempty p (CPlain mempty [] e :| []) :| [])

instance TransformContext (FunctionDef Metadata IndexedType) where
  desugarPatterns =
    \case
      FunctionDef a u w ps e -> do
        e1 <- desugarPatterns e
        (qs, rs) <- listenPatterns (traverse desugarPatterns ps)
        pure (FunctionDef a u w qs (foldr unrollMatch e1 rs))

instance TransformContext (ConstantDef Metadata IndexedType) where
  desugarPatterns =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> desugarPatterns e

instance TransformContext (Definition Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> traverse desugarPatterns f <*> traverse desugarPatterns fs
      DConstant loc name g fs ->
        DConstant loc name <$> desugarPatterns g <*> traverse desugarPatterns fs
      DFold loc n (FoldDef with cs e) ->
        DFold loc n . FoldDef with cs <$> traverse desugarPatterns e
      DUnfold loc n (UnfoldDef with ps d e) ->
        DUnfold loc n . UnfoldDef with ps d <$> traverse desugarPatterns e
      d ->
        pure d

instance TransformContext (Module Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      Module p ns ds ->
        Module p ns <$> traverse desugarPatterns ds
