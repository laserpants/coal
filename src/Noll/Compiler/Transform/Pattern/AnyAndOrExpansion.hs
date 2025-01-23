{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.AnyAndOrExpansion where

import Data.Semigroup (sconcat)
import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Compiler.Transform.Expression (mapMOverExpression)
import Noll.Language (
  Clause (..),
  Expression (..),
  Pattern (..),
 )
import Noll.Utils (concatMapM)

import qualified Noll.Common.List1 as List1

expandOrPatterns :: (Monad m) => Pattern a t -> m (Pattern a t)
expandOrPatterns =
  undefined

bazExpression :: (Monad m) => Expression a t -> m (Expression a t)
bazExpression =
  \case
    EMatch a t e cs -> do
      cs1 <- sconcat <$> traverse bazClause cs
      pure (EMatch a t e cs1)
    EFold a t es cs e -> do
      cs1 <- sconcat <$> traverse bazClause cs
      pure (EFold a t es cs1 e)
    e ->
      mapMOverExpression bazExpression e

bazClause :: (Monad m) => Clause e a t -> m (List1 (Clause e a t))
bazClause =
  \case
    EClause a p cs -> do
      q1 :| qs <- baz p
      pure (EClause a q1 cs :| [EClause a q cs | q <- qs])

baz :: (Monad m) => Pattern a t -> m (List1 (Pattern a t))
baz =
  \case
    PAnnotation a t p -> do
      q1 :| qs <- baz p
      pure (PAnnotation a t q1 :| [PAnnotation a t q | q <- qs])
    PConstructor a ll ps -> do
      qs1 :| qss <- sequence <$> traverse baz ps
      pure (PConstructor a ll qs1 :| [PConstructor a ll qs | qs <- qss])
    POr _ _ p1 p2 -> do
      qs1 <- baz p1
      qs2 <- baz p2
      pure (qs1 <> qs2)
    p@PAny{} ->
      pure (List1.singleton p)
    p@PVariable{} ->
      pure (List1.singleton p)
    p@PLiteral{} ->
      pure (List1.singleton p)
