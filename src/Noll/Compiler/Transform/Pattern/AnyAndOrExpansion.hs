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

--expandOrPatterns :: (Monad m) => Pattern a t -> m (Pattern a t)
--expandOrPatterns =
--  undefined

bazExpression :: (Monad m) => Expression a t -> m (Expression a t)
bazExpression =
  \case
    EMatch a t e cs -> do
      cs1 <- sconcat <$> traverse expandOrPatterns cs
      pure (EMatch a t e cs1)
    EFold a t es cs e -> do
      cs1 <- sconcat <$> traverse expandOrPatterns cs
      pure (EFold a t es cs1 e)
    e ->
      mapMOverExpression bazExpression e

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
      PAnnotation a t p -> do
        q1 :| qs <- expandOrPatterns p
        pure (PAnnotation a t q1 :| [PAnnotation a t q | q <- qs])
      PConstructor a ll ps -> do
        qs1 :| qss <- sequence <$> traverse expandOrPatterns ps
        pure (PConstructor a ll qs1 :| [PConstructor a ll qs | qs <- qss])
      POr _ _ p1 p2 -> do
        qs1 <- expandOrPatterns p1
        qs2 <- expandOrPatterns p2
        pure (qs1 <> qs2)
      p@PAny{} ->
        pure (List1.singleton p)
      p@PVariable{} ->
        pure (List1.singleton p)
      p@PLiteral{} ->
        pure (List1.singleton p)
