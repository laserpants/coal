{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.AsDesugar where

import Noll.Language (Choice (..), Clause (..), Expression (..), Pattern (..))

class AsDesugarContext e where
  desugarAsPatterns :: e -> e

instance AsDesugarContext (Clause a t) where
  desugarAsPatterns =
    \case
      EClause a p cs ->
        undefined

