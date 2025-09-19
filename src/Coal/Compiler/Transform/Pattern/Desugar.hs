{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Pattern.Desugar (
  Sugared (..),
  PatternDesugar (..),
  runPatternDesugar,
  evalPatternDesugar,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (suppliedName)
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
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Type (IndexedType, Type (..), TypeIndex)
import Coal.Language.Type.Kind (Kind (..))
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWS, runRWS, tell)
import Control.Monad.Writer (runWriterT)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Extra (Name)

type NamedPattern a = (Name, Pattern a (Type TypeIndex Kind))

type PatternDesugarStack a = RWS Name [NamedPattern a] Int

newtype PatternDesugar a s = PatternDesugar {patternDesugarStack :: PatternDesugarStack a s}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    , MonadWriter [NamedPattern a]
    )

{-# INLINE evalPatternDesugar #-}
evalPatternDesugar :: Name -> Int -> PatternDesugar c e -> e
evalPatternDesugar r s e = fst (runPatternDesugar r s e)

{-# INLINE runPatternDesugar #-}
runPatternDesugar :: Name -> Int -> PatternDesugar c e -> (e, Int)
runPatternDesugar r s e = (a, s')
 where
  (a, s', _) = runRWS (patternDesugarStack e) r s

class Sugared a s where
  desugarPatterns :: (MonadWriter [NamedPattern a] m, MonadReader Name m, MonadState Int m) => s -> m s

instance (Data a, Monoid a) => Sugared a (Pattern a IndexedType) where
  desugarPatterns =
    \case
      p@PVariable{} ->
        pure p
      p@(PAnnotation _ _ PVariable{}) ->
        pure p
      p -> do
        name <- suppliedName
        tell [(name, p)]
        pure (PVariable mempty (Label (typeOf p) name))

instance (Data a, Monoid a) => Sugared a (Binding Expression a IndexedType) where
  desugarPatterns =
    \case
      BPattern a p e ->
        BPattern a <$> desugarPatterns p <*> desugarPatterns e
      BFunction a name ps e ->
        BFunction a name <$> traverse desugarPatterns ps <*> desugarPatterns e

instance (Data s, Monoid s) => Sugared a (Expression s IndexedType) where
  desugarPatterns = transformM go
   where
    go =
      \case
        ELet a gs e1 -> do
          d1 <- desugarPatterns e1
          (hs, ps) <- runWriterT (traverse desugarPatterns gs)
          pure (ELet a hs (foldr unrollMatch d1 ps))
        ERecursiveLet a p e1 e2 -> do
          d1 <- desugarPatterns e1
          d2 <- desugarPatterns e2
          (q, ps) <- runWriterT (desugarPatterns p)
          pure (ERecursiveLet a q d1 (foldr unrollMatch d2 ps))
        ELambda a ps e -> do
          e1 <- desugarPatterns e
          (qs, rs) <- runWriterT (traverse desugarPatterns ps)
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

instance (Data s, Monoid s) => Sugared a (FunctionDef s IndexedType) where
  desugarPatterns =
    \case
      FunctionDef a u w ps e -> do
        e1 <- desugarPatterns e
        (qs, rs) <- runWriterT (traverse desugarPatterns ps)
        pure (FunctionDef a u w qs (foldr unrollMatch e1 rs))

instance (Data s, Monoid s) => Sugared a (ConstantDef s IndexedType) where
  desugarPatterns =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> desugarPatterns e

instance (Data s, Monoid s) => Sugared a (Definition s Kind IndexedType) where
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

instance (Data s, Monoid s) => Sugared a (Module s Kind IndexedType) where
  desugarPatterns =
    \case
      Module p ns ds ->
        Module p ns <$> traverse desugarPatterns ds
