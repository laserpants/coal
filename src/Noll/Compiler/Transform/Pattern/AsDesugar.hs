{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.AsDesugar where

import Lang.Common.List1 (List1, NonEmpty (..))
import Noll.Language (Choice (..), Clause (..), Expression (..), Pattern (..))

class AsDesugarContext e where
  desugarAsPatterns :: e -> e

instance (AsDesugarContext e) => AsDesugarContext [e] where
  desugarAsPatterns = fmap desugarAsPatterns

instance (AsDesugarContext e) => AsDesugarContext (NonEmpty e) where
  desugarAsPatterns = fmap desugarAsPatterns

instance AsDesugarContext (Clause a t) where
  desugarAsPatterns =
    \case
      EClause a p cs ->
        undefined

instance AsDesugarContext (Choice Expression a t) where
  desugarAsPatterns =
    \case
      CPlain a gs e ->
        undefined
      CLambda{} ->
        error "TODO"

instance AsDesugarContext (Expression a t) where
  desugarAsPatterns = undefined
