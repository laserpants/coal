{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.Expansion where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, State, evalState)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import Noll.Common.List1 (List1, NonEmpty ((:|)))
import Noll.Common.Supply (supplied, suppliedName)
import Noll.Compiler.Transform.Expression (mapMOverExpression)
import Noll.Label (Label (..))
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..))
import Noll.Language.HasType (HasType (..))
import Noll.Language.Module.Constant (Constant (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Language.Module.Object (Object (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Type (Type (..))
import Noll.Utils (Name, foldrM)

import qualified Data.Text as Text

type NamedPattern a o k = (Name, Pattern a (Type o k))

type PatternExpansionStack a o k = WriterT [NamedPattern a o k] (ReaderT Name (State Int))

newtype PatternExpansion a o k e = PatternExpansion {patternExpansionStack :: PatternExpansionStack a o k e}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    , MonadWriter [NamedPattern a o k]
    )

{-# INLINE runPatternExpansion #-}
runPatternExpansion :: Name -> Int -> PatternExpansion a o k e -> e
runPatternExpansion r s e = fst (evalState (runReaderT (runWriterT (patternExpansionStack e)) r) s)

class Expandable a o k e | e -> a, e -> o k where
  expandPatterns ::
    (MonadWriter [NamedPattern a o k] m, MonadReader Name m, MonadState Int m) =>
    e ->
    m e

instance (Monoid a) => Expandable a o k (Pattern a (Type o k)) where
  expandPatterns =
    \case
      p@PVariable{} ->
        pure p
      p@(PAnnotation _ _ PVariable{}) ->
        pure p
      p -> do
        name <- suppliedName
        tell [(name, p)]
        pure (PVariable mempty (Label (typeOf p) name))

instance (Monoid a) => Expandable a o k (Binding Expression a (Type o k)) where
  expandPatterns =
    \case
      BPattern a p e ->
        BPattern a <$> expandPatterns p <*> expandPatterns e
      BFunction a name ps e ->
        BFunction a name <$> traverse expandPatterns ps <*> expandPatterns e

instance (Monoid a) => Expandable a o k (Expression a (Type o k)) where
  expandPatterns =
    \case
      ELet a gs e1 -> do
        e2 <- expandPatterns e1
        (hs, ps) <- runWriterT (traverse expandPatterns gs)
        pure (ELet a hs (foldr unrollMatch e2 ps))
      ERecursiveLet a p e1 e2 -> do
        (p1, ps) <- runWriterT (expandPatterns p)
        q1 <- BPattern a p1 <$> expandPatterns e1
        pure (ELet a (q1 :| []) (foldr unrollMatch e2 ps))
      ELambda a ps e -> do
        e1 <- expandPatterns e
        (qs, ps) <- runWriterT (traverse expandPatterns ps)
        pure (ELambda a qs (foldr unrollMatch e1 ps))
      e ->
        mapMOverExpression expandPatterns e

unrollMatch :: (Monoid a) => (Name, Pattern a (Type o k)) -> Expression a (Type o k) -> Expression a (Type o k)
unrollMatch (name, p) e =
  EMatch
    mempty
    (typeOf e)
    (EVariable mempty (Label (typeOf p) name))
    (EClause mempty p (CPlain mempty [] e :| []) :| [])

instance (Monoid a) => Expandable a o k (Function Expression a (Type o k)) where
  expandPatterns =
    \case
      Function a u ps e -> do
        e1 <- expandPatterns e
        (qs, ps) <- runWriterT (traverse expandPatterns ps)
        pure (Function a u qs (foldr unrollMatch e1 ps))

instance (Monoid a) => Expandable a o k (Constant Expression a (Type o k)) where
  expandPatterns =
    \case
      Constant a u e ->
        Constant a u <$> expandPatterns e

instance (Monoid a) => Expandable a o k (Object a k (Type o k)) where
  expandPatterns =
    \case
      DFunction name f ->
        DFunction name <$> expandPatterns f
      DConstant name g ->
        DConstant name <$> expandPatterns g
      d ->
        pure d
