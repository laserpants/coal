{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.AnyOrExpansion where

import Noll.Language (
  Pattern (..),
 )

expandAnyOrExpansion :: (Monad m) => Pattern a t -> m (Pattern a t)
expandAnyOrExpansion =
  \case
    PAnnotation a _ _ ->
      undefined
    PAny a t ->
      undefined
    POr a t p1 p2 ->
      undefined
