{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

-- FIXME
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
import Coal.Language.Type (Type (..))
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWS, runRWS, tell)
import Control.Monad.Writer (runWriterT)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Extra (Name)

type NamedPattern c o k = (Name, Pattern c (Type o k))

type PatternDesugarStack c o k = RWS Name [NamedPattern c o k] Int

newtype PatternDesugar c o k e = PatternDesugar {patternDesugarStack :: PatternDesugarStack c o k e}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    , MonadWriter [NamedPattern c o k]
    )

{-# INLINE evalPatternDesugar #-}
evalPatternDesugar :: Name -> Int -> PatternDesugar c o k e -> e
evalPatternDesugar r s e = fst (runPatternDesugar r s e)

{-# INLINE runPatternDesugar #-}
runPatternDesugar :: Name -> Int -> PatternDesugar c o k e -> (e, Int)
runPatternDesugar r s e = (a, s')
 where
  (a, s', _) = runRWS (patternDesugarStack e) r s

class Sugared c o k e | e -> c, e -> o k where
  desugarPatterns ::
    (MonadWriter [NamedPattern c o k] m, MonadReader Name m, MonadState Int m) =>
    e ->
    m e

instance (Monoid c, Data c, Data k, Data (o k), Typeable o) => Sugared c o k (Pattern c (Type o k)) where
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

instance (Monoid c, Data c, Data k, Typeable o, Data (o k)) => Sugared c o k (Binding Expression c (Type o k)) where
  desugarPatterns =
    \case
      BPattern a p e ->
        BPattern a <$> desugarPatterns p <*> desugarPatterns e
      BFunction a name ps e ->
        BFunction a name <$> traverse desugarPatterns ps <*> desugarPatterns e

instance (Monoid c, Data c, Data k, Typeable o, Data (o k)) => Sugared c o k (Expression c (Type o k)) where
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

unrollMatch :: (Monoid c, Data c, Data k, Typeable o, Data (o k)) => (Name, Pattern c (Type o k)) -> Expression c (Type o k) -> Expression c (Type o k)
unrollMatch (name, p) e =
  EMatch
    mempty
    (typeOf e)
    (EVariable mempty (Label (typeOf p) name))
    (EClause mempty p (CPlain mempty [] e :| []) :| [])

instance (Monoid c, Data c, Data k, Typeable o, Data (o k)) => Sugared c o k (FunctionDef c (Type o k)) where
  desugarPatterns =
    \case
      FunctionDef a u w ps e -> do
        e1 <- desugarPatterns e
        (qs, rs) <- runWriterT (traverse desugarPatterns ps)
        pure (FunctionDef a u w qs (foldr unrollMatch e1 rs))

instance (Monoid c, Data c, Data k, Typeable o, Data (o k)) => Sugared c o k (ConstantDef c (Type o k)) where
  desugarPatterns =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> desugarPatterns e

instance (Monoid c, Data k, Data c, Data (o k), Typeable o) => Sugared c o k (Definition c k (Type o k)) where
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

instance (Monoid c, Data k, Data c, Data (o k), Typeable o) => Sugared c o k (Module c k (Type o k)) where
  desugarPatterns =
    \case
      Module p ns ds ->
        Module p ns <$> traverse desugarPatterns ds
