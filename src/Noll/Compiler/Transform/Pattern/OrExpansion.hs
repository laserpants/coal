{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.OrExpansion (
  OrPattern (..),
  expandExpressionOrPatterns,
  baz2,
) where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.Semigroup (sconcat)
import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Language (
  Clause (..),
  Expression (..),
  Module (..),
  Pattern (..),
 )

import qualified Noll.Common.List1 as List1

baz2 :: forall m a k t. (Monad m, Data a, Data k, Ord k, Data t) => Module a k t -> m (Module a k t)
baz2 = transformBiM (expandExpressionOrPatterns :: Expression a t -> m (Expression a t))

expandExpressionOrPatterns :: (Monad m) => Expression a t -> m (Expression a t)
expandExpressionOrPatterns =
  \case
    EMatch a t e cs ->
      EMatch a t e . sconcat <$> traverse expandOrPatterns cs
    EFold a t es cs e ->
      EFold a t es . sconcat <$> traverse expandOrPatterns cs <*> pure e
    e ->
      pure e

class OrPattern a where
  expandOrPatterns :: (Monad m) => a -> m (List1 a)

instance OrPattern (Clause e a t) where
  expandOrPatterns =
    \case
      EClause a p cs -> do
        q1 :| qs <- expandOrPatterns p
        pure (EClause a q1 cs :| [EClause a q cs | q <- qs])

instance OrPattern (Pattern a t) where
  expandOrPatterns =
    \case
      POr _ _ p1 p2 -> do
        qs1 <- expandOrPatterns p1
        qs2 <- expandOrPatterns p2
        pure (qs1 <> qs2)
      PAnnotation a t p -> do
        q1 :| qs <- expandOrPatterns p
        pure (PAnnotation a t q1 :| [PAnnotation a t q | q <- qs])
      PConstructor a ll ps -> do
        qs1 :| qss <- sequence <$> traverse expandOrPatterns ps
        pure (PConstructor a ll qs1 :| [PConstructor a ll qs | qs <- qss])
      PListLiteral a t ps -> do
        qs1 :| qss <- sequence <$> traverse expandOrPatterns ps
        pure (PListLiteral a t qs1 :| [PListLiteral a t qs | qs <- qss])
      PListCons{} ->
        error "TODO"
      PRecord{} ->
        error "TODO"
      p@PAny{} ->
        pure (List1.singleton p)
      p@PVariable{} ->
        pure (List1.singleton p)
      p@PLiteral{} ->
        pure (List1.singleton p)
      p@PAtVariable{} ->
        pure (List1.singleton p)
      p@PShorthand{} ->
        pure (List1.singleton p)
