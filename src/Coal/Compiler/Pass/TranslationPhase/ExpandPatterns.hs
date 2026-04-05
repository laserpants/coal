{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.ExpandPatterns (passExpandPatterns) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (listenPatterns, tellPatterns1)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..))
import Coal.Language.HasType (HasType (..), foldTypeOf)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Definition (Definition (..))
import Coal.Language.Module.Definition.Constant (ConstantDefinition (..))
import Coal.Language.Module.Definition.Function (FunctionDefinition (..))
import Coal.Language.Module.Definition.Instance (InstanceDefinition (..))
import Coal.Language.Pattern (IndexedPattern, Pattern (..))
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT)
import Control.Monad.Trans (lift)
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Extras (Name)

passExpandPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passExpandPatterns = Pass{runPass = desugarPatterns}

class TransformContext s where
  desugarPatterns :: (Monad m) => s -> CompilerT Metadata (ProtoCompilerT m Metadata) s

instance TransformContext (IndexedPattern Metadata) where
  desugarPatterns =
    \case
      p@PVariable{} ->
        pure p
      p@(PAnnotation _ _ PVariable{}) ->
        pure p
      PShorthand loc (Label t name) ->
        desugarPatterns (PVariable loc (Label t name))
      p -> do
        name <- lift $ supplied (freshName "v")
        tellPatterns1 (name, p)
        pure (PVariable mempty (Label (typeOf p) name))

instance TransformContext (Binding Expression Metadata () IndexedType) where
  desugarPatterns =
    \case
      BPattern a p e ->
        BPattern a <$> desugarPatterns p <*> desugarPatterns e
      BFunction a name ps e ->
        desugarPatterns
          ( BPattern
              a
              (PVariable mempty (Label (foldTypeOf e ps) name))
              (ELambda mempty ps e)
          )

instance TransformContext (Expression Metadata () IndexedType) where
  desugarPatterns = go
   where
    go =
      \case
        ELet a gs e1 -> do
          d1 <- desugarPatterns e1
          (hs, ps) <- listenPatterns (traverse desugarPatterns gs)
          pure (ELet a hs (foldr (unrollMatch a) d1 ps))
        ERecursiveLet a p e1 e2 -> do
          d1 <- desugarPatterns e1
          d2 <- desugarPatterns e2
          (q, ps) <- listenPatterns (desugarPatterns p)
          pure (ERecursiveLet a q d1 (foldr (unrollMatch a) d2 ps))
        ELambda a ps e -> do
          e1 <- desugarPatterns e
          (qs, rs) <- listenPatterns (traverse desugarPatterns ps)
          pure (ELambda a qs (foldr (unrollMatch a) e1 rs))
        e ->
          descendM go e

unrollMatch :: Metadata -> (Name, Pattern Metadata () IndexedType) -> Expression Metadata () IndexedType -> Expression Metadata () IndexedType
unrollMatch loc (name, p) e =
  EMatch
    loc
    (typeOf e)
    (EVariable mempty (Label (typeOf p) name))
    (EClause loc p (CPlain mempty [] e :| []) :| [])

instance TransformContext (FunctionDefinition Metadata IndexedType) where
  desugarPatterns =
    \case
      FunctionDefinition a u w ps e -> do
        e1 <- desugarPatterns e
        (qs, rs) <- listenPatterns (traverse desugarPatterns ps)
        pure (FunctionDefinition a u w qs (foldr (unrollMatch a) e1 rs))

instance TransformContext (ConstantDefinition Metadata IndexedType) where
  desugarPatterns =
    \case
      ConstantDefinition a u w e ->
        ConstantDefinition a u w <$> desugarPatterns e

instance TransformContext (Definition Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> traverse desugarPatterns f <*> traverse desugarPatterns fs
      DConstant loc name g fs ->
        DConstant loc name <$> desugarPatterns g <*> traverse desugarPatterns fs
      DInstance loc n (InstanceDefinition ts pt ds) ->
        DInstance loc n . InstanceDefinition ts pt <$> traverse desugarPatterns ds
      d ->
        pure d

instance TransformContext (Module Metadata Kind IndexedType) where
  desugarPatterns =
    \case
      Module p ns ds ->
        Module p ns <$> traverse desugarPatterns ds
