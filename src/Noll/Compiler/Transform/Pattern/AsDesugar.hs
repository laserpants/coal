{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.AsDesugar where

import Control.Monad.Writer
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descend, transformM)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Common.Label (Label (..))
import Noll.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Noll.Language.Module (Module (..))
import Noll.Language.Module.Constant (Constant (..))
import Noll.Language.Module.Definition (Definition (..))
import Noll.Language.Module.Function (Function (..))

class AsDesugarContext e where
  desugarAsPatterns :: e -> e

instance (AsDesugarContext e) => AsDesugarContext [e] where
  desugarAsPatterns = fmap desugarAsPatterns

instance (AsDesugarContext e) => AsDesugarContext (NonEmpty e) where
  desugarAsPatterns = fmap desugarAsPatterns

instance (Data a, Data t, Monoid a) => AsDesugarContext (Expression a t) where
  desugarAsPatterns =
    \case
      EMatch a t e cs ->
        EMatch a t e (fmap (desugarClause t) cs)
      e ->
        descend desugarAsPatterns e

desugarClause :: (Monoid a, Data a, Data t) => t -> Clause a t -> Clause a t
desugarClause t cl@(EClause a p cs) =
  case ps of
    [] ->
      cl
    _ ->
      EClause a q (foldr go cs ps)
 where
  (q, ps) =
    runWriter (transformM collectAsPatterns p)
  go (ll, p1) cs1 =
    CPlain
      mempty
      []
      ( EMatch
          mempty
          t
          (EVariable mempty ll)
          (EClause mempty p1 cs1 :| [])
      )
      :| []

collectAsPatterns :: Pattern a t -> Writer [(Label t, Pattern a t)] (Pattern a t)
collectAsPatterns =
  \case
    PAs a ll p -> do
      tell [(ll, p)]
      pure (PVariable a ll)
    p ->
      pure p

instance (Data a, Data t, Monoid a) => AsDesugarContext (Constant Expression a t) where
  desugarAsPatterns =
    \case
      Constant a u e ->
        Constant a u (desugarAsPatterns e)

instance (Data a, Data t, Monoid a) => AsDesugarContext (Function Expression a t) where
  desugarAsPatterns =
    \case
      Function a u ps e ->
        case ps1 of
          [] ->
            Function a u ps (desugarAsPatterns e)
          _ ->
            Function a u qs (foldr go (desugarAsPatterns e) ps1)
       where
        (qs, ps1) =
          runWriter (traverse (transformM collectAsPatterns) ps)
        go (ll@(Label t _), p1) e1 =
          EMatch
            mempty
            t -- TODO
            (EVariable mempty ll)
            (EClause mempty p1 (CPlain mempty [] e1 :| []) :| [])

instance (Data a, Data t, Monoid a) => AsDesugarContext (Definition a k t) where
  desugarAsPatterns =
    \case
      DAnnotation u d ->
        DAnnotation u (desugarAsPatterns d)
      DFunction name f ->
        DFunction name (desugarAsPatterns f)
      DConstant name g ->
        DConstant name (desugarAsPatterns g)
      d ->
        d

instance (Data a, Data t, Monoid a) => AsDesugarContext (Module a k t) where
  desugarAsPatterns =
    \case
      Module p ns o ->
        Module p ns (desugarAsPatterns o)
