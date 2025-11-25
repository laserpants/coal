{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.ExpandAsPatterns (passExpandAsPatterns) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Pass
import Coal.Language
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Definition (Definition (..))
import Coal.Language.Module.Definition.Constant (ConstantDef (..))
import Coal.Language.Module.Definition.Fold (FoldDef (..))
import Coal.Language.Module.Definition.Unfold (UnfoldDef (..))
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descend, transformM)
import Data.List.NonEmpty (NonEmpty (..))

passExpandAsPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passExpandAsPatterns =
  Pass
    { passName = "ExpandAsPatterns"
    , runPass = pure . expandAsPatterns
    }

class TransformContext e where
  expandAsPatterns :: e -> e

instance (TransformContext e) => TransformContext [e] where
  expandAsPatterns = fmap expandAsPatterns

instance (TransformContext e) => TransformContext (NonEmpty e) where
  expandAsPatterns = fmap expandAsPatterns

instance (Data a, Data t, Monoid a) => TransformContext (Expression a t) where
  expandAsPatterns =
    \case
      EMatch a t e cs ->
        EMatch a t e (fmap (expandClause t) cs)
      e ->
        descend expandAsPatterns e

expandClause :: (Monoid a, Data a, Data t) => t -> Clause a t -> Clause a t
expandClause t cl@(EClause a p cs) =
  case ps of
    [] -> cl
    _ -> EClause a q (foldr go cs ps)
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

instance (Data a, Data t, Monoid a) => TransformContext (ConstantDef a t) where
  expandAsPatterns =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w (expandAsPatterns e)

instance (Data a, Data t, Monoid a) => TransformContext (Definition a k t) where
  expandAsPatterns =
    \case
      DConstant loc name g fs ->
        DConstant loc name (expandAsPatterns g) (expandAsPatterns <$> fs)
      DFold loc n (FoldDef with cs e) ->
        DFold loc n (FoldDef with cs (expandAsPatterns <$> e))
      DUnfold loc n (UnfoldDef with ps d me) ->
        DUnfold loc n (UnfoldDef with ps d (expandAsPatterns <$> me))
      d ->
        d

instance (Data a, Data t, Monoid a) => TransformContext (Module a k t) where
  expandAsPatterns =
    \case
      Module p ns o ->
        Module p ns (expandAsPatterns o)
