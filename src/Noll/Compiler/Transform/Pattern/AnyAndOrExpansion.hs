{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.AnyAndOrExpansion where

import Noll.Language (
  Pattern (..),
 )

expandOrPatterns :: (Monad m) => Pattern a t -> m (Pattern a t)
expandOrPatterns =
  undefined

baz :: (Monad m) => Pattern a t -> m [Pattern a t]
baz =
  \case
    PAnnotation a t p -> do
      qs <- baz p
      pure [PAnnotation a t q | q <- qs]
    PConstructor a ll ps -> do
      qss <- sequence <$> traverse baz ps
      pure [PConstructor a ll qs | qs <- qss]
    POr _ _ p1 p2 -> do 
      qs1 <- baz p1
      qs2 <- baz p2
      pure (qs1 <> qs2)
    p@PAny{} ->
      pure [p]
    p@PVariable{} ->
      pure [p]
    p@PLiteral{} ->
      pure [p]

