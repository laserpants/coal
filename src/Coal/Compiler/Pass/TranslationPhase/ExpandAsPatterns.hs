{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.ExpandAsPatterns (
  passExpandAsPatterns,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Definition (Definition (DLet), LetDefinition (..))
import Coal.Language.Module (Module (..))
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descend, transformM)
import Data.List.NonEmpty (NonEmpty (..))

passExpandAsPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passExpandAsPatterns = Pass{runPass = passImpl}

passImpl :: (Monad m, Monoid a, Data a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
passImpl = return . expandAsPatterns

class ExpandContext e where
  expandAsPatterns :: e -> e

instance (ExpandContext e) => ExpandContext [e] where
  expandAsPatterns = fmap expandAsPatterns

instance (ExpandContext e) => ExpandContext (NonEmpty e) where
  expandAsPatterns = fmap expandAsPatterns

instance (Data a, Data k, Data t, Monoid a) => ExpandContext (Expression a k t) where
  expandAsPatterns =
    \case
      EMatch a t e cs ->
        EMatch a t e (fmap (expandClause t) cs)
      e ->
        descend expandAsPatterns e

instance (Data a, Data k, Data t, Monoid a) => ExpandContext (Choice Expression a k t) where
  expandAsPatterns =
    \case
      CPlain a gs e ->
        CPlain a (fmap expandAsPatterns gs) (expandAsPatterns e)

instance (Data a, Data k, Data t, Monoid a) => ExpandContext (Guard Expression a k t) where
  expandAsPatterns =
    \case
      CGuard e ->
        CGuard (expandAsPatterns e)

instance (Data a, Data k, Data t, Monoid a) => ExpandContext (Binding Expression a k t) where
  expandAsPatterns =
    \case
      BPattern a p e ->
        BPattern a p (expandAsPatterns e)
      BFunction a name ps e ->
        BFunction a name ps (expandAsPatterns e)

expandClause :: (Monoid a, Data a, Data k, Data t) => t -> Clause a k t -> Clause a k t
expandClause t (EClause a p cs) =
  case ps of
    [] ->
      EClause a q (expandAsPatterns cs)
    _ ->
      EClause a q (foldr go (expandAsPatterns cs) ps)
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

collectAsPatterns :: Pattern a k t -> Writer [(Label t, Pattern a k t)] (Pattern a k t)
collectAsPatterns =
  \case
    PAs a ll p -> do
      tell [(ll, p)]
      return (PVariable a ll)
    p ->
      return p

instance (Data a, Data k, Data t, Monoid a) => ExpandContext (Module a k t) where
  expandAsPatterns =
    \case
      Module{..} ->
        Module
          { moduleDefinitions = fmap expandAsPatterns moduleDefinitions
          , ..
          }

instance (Data a, Data k, Data t, Monoid a) => ExpandContext (Definition a k t) where
  expandAsPatterns =
    \case
      DLet loc name def ->
        DLet loc name (expandAsPatterns def)
      d ->
        d

instance (Data a, Data k, Data t, Monoid a) => ExpandContext (LetDefinition a k t) where
  expandAsPatterns =
    \case
      LetDefinition{..} ->
        LetDefinition
          { letDefinitionExpression = expandAsPatterns letDefinitionExpression
          , ..
          }
