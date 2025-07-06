{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.AsDesugar where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Control.Monad.Writer
import Lang.Utils (Name)
import Lang.Label (Label (..))
import Lang.Common.List1 (List1, NonEmpty (..))
import Noll.Language (Choice (..), Clause (..), Expression (..), Pattern (..))

class AsDesugarContext e where
  desugarAsPatterns :: e -> e

instance (AsDesugarContext e) => AsDesugarContext [e] where
  desugarAsPatterns = fmap desugarAsPatterns

instance (AsDesugarContext e) => AsDesugarContext (NonEmpty e) where
  desugarAsPatterns = fmap desugarAsPatterns

instance (Data a, Data t) => AsDesugarContext (Clause a t) where
  desugarAsPatterns =
    \case
      clause@(EClause a p cs) -> do
        let (q, ps) = runWriter (transformM collectAsPatterns p)
         in 
          case ps of
            [] ->
              clause
            _ ->
              EClause a q (bananx ps cs)

instance (Data a, Data t) => AsDesugarContext (Choice Expression a t) where
  desugarAsPatterns =
    \case
      CPlain a gs e ->
        undefined
      CLambda{} ->
        error "TODO"

instance (Data a, Data t) => AsDesugarContext (Expression a t) where
  desugarAsPatterns = undefined

bananx :: [(Name, Pattern a t)] -> List1 (Choice Expression a t) -> List1 (Choice Expression a t)
bananx = undefined

collectAsPatterns :: Pattern a t -> Writer [(Name, Pattern a t)] (Pattern a t)
collectAsPatterns =
  \case
    PAs a ll@(Label _ name) p -> do
      tell [(name, p)]
      pure (PVariable a ll)
    p ->
      pure p
