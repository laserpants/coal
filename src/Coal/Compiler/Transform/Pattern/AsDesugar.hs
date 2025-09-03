{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Pattern.AsDesugar where

import Coal.Common.Label (Label (..))
import Coal.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Definition (Definition (..))
import Coal.Language.Module.Definition.Constant (ConstantDef (..))
import Coal.Language.Module.Definition.Fold (FoldDef (..))
import Coal.Language.Module.Definition.Function (FunctionDef (..))
import Control.Monad.Writer
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descend, transformM)
import Data.List.NonEmpty (NonEmpty (..))

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

instance (Data a, Data t, Monoid a) => AsDesugarContext (ConstantDef a t) where
  desugarAsPatterns =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w (desugarAsPatterns e)

instance (Data a, Data t, Monoid a) => AsDesugarContext (FunctionDef a t) where
  desugarAsPatterns =
    \case
      FunctionDef a u w ps e ->
        case ps1 of
          [] ->
            FunctionDef a u w ps (desugarAsPatterns e)
          _ ->
            FunctionDef a u w qs (foldr go (desugarAsPatterns e) ps1)
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
      DFunction loc name f fs ->
        DFunction loc name (desugarAsPatterns f) (desugarAsPatterns <$> fs)
      DConstant loc name g fs ->
        DConstant loc name (desugarAsPatterns g) (desugarAsPatterns <$> fs)
      DFold loc n (FoldDef with cs e) ->
        DFold loc n (FoldDef with cs (desugarAsPatterns <$> e))
      DUnfold{} ->
        error "TODO"
      d ->
        d

instance (Data a, Data t, Monoid a) => AsDesugarContext (Module a k t) where
  desugarAsPatterns =
    \case
      Module p ns o ->
        Module p ns (desugarAsPatterns o)
