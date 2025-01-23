{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.AnyAndOrExpansion where

import Noll.Language (
  Pattern (..),
 )

expandOrPatterns :: (Monad m) => Pattern a t -> m (Pattern a t)
expandOrPatterns =
  \case
    PAnnotation a _ _ ->
      undefined
    PAny a t ->
      undefined
    POr a t p1 p2 ->
      undefined
