{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Pattern.OrExpansion (
  OrPattern (..),
  compileOrPatterns,
  expandExpression,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..))
import Coal.Language (Clause (..), Expression (..), Pattern (..))
import Coal.Language.Module (Module (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.Semigroup (sconcat)
import Extra (Map, traverseM)

import qualified Coal.Common.List1 as List1

compileOrPatterns :: forall m a k t. (Monad m, Data a, Data k, Data t) => Module a k t -> m (Module a k t)
compileOrPatterns = transformBiM (expandExpression :: Expression a t -> m (Expression a t))

expandExpression :: (Monad m) => Expression a t -> m (Expression a t)
expandExpression =
  \case
    EMatch a t e cs ->
      EMatch a t e . sconcat <$> traverse expandOrPatterns cs
    EFold a t es cs e ->
      EFold a t es . sconcat <$> traverse expandOrPatterns cs <*> pure e
    e ->
      pure e

class OrPattern a where
  expandOrPatterns :: (Monad m) => a -> m (List1 a)

instance (OrPattern a) => OrPattern [a] where
  expandOrPatterns = traverseM expandOrPatterns

instance (OrPattern a) => OrPattern (List1 a) where
  expandOrPatterns = traverseM expandOrPatterns

instance (OrPattern a) => OrPattern (Map k a) where
  expandOrPatterns = traverseM expandOrPatterns

instance (OrPattern a) => OrPattern (Maybe a) where
  expandOrPatterns = traverseM expandOrPatterns

instance OrPattern (Clause a t) where
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
      PConstructor a (Label t name) ps -> do
        qs1 :| qss <- expandOrPatterns ps
        pure (PConstructor a (Label t name) qs1 :| [PConstructor a (Label t name) qs | qs <- qss])
      PListLiteral a t ps -> do
        qs1 :| qss <- expandOrPatterns ps
        pure (PListLiteral a t qs1 :| [PListLiteral a t qs | qs <- qss])
      q@(PListCons a t p1 p2) -> do
        pure (List1.singleton q)
      -- TODO
      -- q1 :| qs1 <- expandOrPatterns p1
      -- q2 :| qs2 <- expandOrPatterns p2
      -- error "TODO"
      q@(PRecord a t d p) -> do
        pure (List1.singleton q)
      -- TODO
      -- d1 :| ds <- expandOrPatterns d
      -- q1 :| qs <- expandOrPatterns p
      -- error "TODO"
      PAs a ll p -> do
        q1 :| qs <- expandOrPatterns p
        pure (PAs a ll q1 :| [PAs a ll q | q <- qs])
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
      _ ->
        error "TODO"
