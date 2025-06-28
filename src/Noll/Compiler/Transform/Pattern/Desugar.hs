{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.Desugar (
  Sugared (..),
  PatternDesugar (..),
  runPatternDesugar,
  evalPatternDesugar,
) where

import Control.Monad.RWS (RWS, evalRWS, runRWS, MonadReader, MonadState, MonadWriter, tell)
import Control.Monad.Writer (runWriterT)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transformM)
import Lang.Common.List1 (NonEmpty ((:|)))
import Lang.Common.Supply (suppliedName)
import Lang.Label (Label (..))
import Lang.Utils (Name)
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..))
import Noll.Language.HasType (HasType (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Type (Type (..))
import Noll.Module (Module (..))
import Noll.Module.Constant (Constant (..))
import Noll.Module.Definition (Definition (..))
import Noll.Module.Function (Function (..))

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

instance (Monoid c, Data c, Data k, Typeable o, Data (o k)) => Sugared c o k (Function Expression c (Type o k)) where
  desugarPatterns =
    \case
      Function a u ps e -> do
        e1 <- desugarPatterns e
        (qs, rs) <- runWriterT (traverse desugarPatterns ps)
        pure (Function a u qs (foldr unrollMatch e1 rs))

instance (Monoid c, Data c, Data k, Typeable o, Data (o k)) => Sugared c o k (Constant Expression c (Type o k)) where
  desugarPatterns =
    \case
      Constant a u e ->
        Constant a u <$> desugarPatterns e

instance (Monoid c, Data k, Data c, Data (o k), Typeable o) => Sugared c o k (Definition c k (Type o k)) where
  desugarPatterns =
    \case
      DAnnotation u d ->
        DAnnotation u <$> desugarPatterns d
      DFunction name f ->
        DFunction name <$> desugarPatterns f
      DConstant name g ->
        DConstant name <$> desugarPatterns g
      d ->
        pure d

instance (Monoid c, Data k, Data c, Data (o k), Typeable o) => Sugared c o k (Module c k (Type o k)) where
  desugarPatterns =
    \case
      Module p ns ds ->
        Module p ns <$> traverse desugarPatterns ds
