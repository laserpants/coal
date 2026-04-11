{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.ExpandAsPatterns (passExpandAsPatterns) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Pass
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descend, transformM)
import Data.List.NonEmpty (NonEmpty (..))

passExpandAsPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passExpandAsPatterns = Pass{runPass = bork}

bork :: (Monad m, Monoid a, Data a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
bork m = do
  let xx = expandAsPatterns m
  return xx

class TransformContext e where
  expandAsPatterns :: e -> e

instance (TransformContext e) => TransformContext [e] where
  expandAsPatterns = fmap expandAsPatterns

instance (TransformContext e) => TransformContext (NonEmpty e) where
  expandAsPatterns = fmap expandAsPatterns

-- instance (Data a, Data t, Monoid a) => TransformContext (Expression a () t) where
--  expandAsPatterns =
--    \case
--      EMatch a t e cs ->
--        EMatch a t e (fmap (expandClause t) cs)
--      e ->
--        descend expandAsPatterns e

instance (Data a, Data k, Data t, Monoid a) => TransformContext (Expression a k t) where
  expandAsPatterns =
    \case
      EMatch a t e cs ->
        EMatch a t e (fmap (expandClause t) cs)
      e ->
        descend expandAsPatterns e

instance (Data a, Data k, Data t, Monoid a) => TransformContext (Choice Expression a k t) where
  expandAsPatterns =
    \case
      CPlain a gs e ->
        CPlain a (fmap expandAsPatterns gs) (expandAsPatterns e)

-- instance (Data a, Data t, Monoid a) => TransformContext (Choice Expression a () t) where
--  expandAsPatterns =
--    \case
--      CPlain a gs e ->
--        CPlain a (fmap expandAsPatterns gs) (expandAsPatterns e)

-- instance (Data a, Data t, Monoid a) => TransformContext (Guard Expression a () t) where
--  expandAsPatterns =
--    \case
--      CGuard e ->
--        CGuard (expandAsPatterns e)

instance (Data a, Data k, Data t, Monoid a) => TransformContext (Guard Expression a k t) where
  expandAsPatterns =
    \case
      CGuard e ->
        CGuard (expandAsPatterns e)

-- instance (Data a, Data t, Monoid a) => TransformContext (Binding Expression a () t) where
--  expandAsPatterns =
--    \case
--      BPattern a p e ->
--        BPattern a p (expandAsPatterns e)
--      BFunction a name ps e ->
--        BFunction a name ps (expandAsPatterns e)

instance (Data a, Data k, Data t, Monoid a) => TransformContext (Binding Expression a k t) where
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
      EClause a q cs'
    _ ->
      EClause a q (foldr go cs' ps)
 where
  cs' = expandAsPatterns cs
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
      pure (PVariable a ll)
    p ->
      pure p

-- instance (Data a, Data t, Monoid a) => TransformContext (ConstantDefinition a t) where
--  expandAsPatterns =
--    \case
--      ConstantDefinition a u w e ->
--        ConstantDefinition a u w (expandAsPatterns e)
--
-- instance (Data a, Data t, Monoid a) => TransformContext (Definition a k t) where
--  expandAsPatterns =
--    \case
--      DConstant loc name g fs ->
--        DConstant loc name (expandAsPatterns g) (expandAsPatterns <$> fs)
--      d ->
--        d
--
-- instance (Data a, Data t, Monoid a) => TransformContext (Module a k t) where
--  expandAsPatterns =
--    \case
--      Module p ns o ->
--        Module p ns (expandAsPatterns o)

instance (Data a, Data k, Data t, Monoid a) => TransformContext (Module a k t) where
  expandAsPatterns =
    \case
      Module{..} ->
        Module
          { protoOmoduleDefinitions = fmap expandAsPatterns protoOmoduleDefinitions
          , ..
          }

instance (Data a, Data k, Data t, Monoid a) => TransformContext (Definition a k t) where
  expandAsPatterns =
    \case
      DLet loc name def ->
        DLet loc name (expandAsPatterns def)
      d ->
        d

instance (Data a, Data k, Data t, Monoid a) => TransformContext (LetDefinition a k t) where
  expandAsPatterns =
    \case
      LetDefinition{..} ->
        LetDefinition
          { protoOletDefinitionExpression = expandAsPatterns protoOletDefinitionExpression
          , ..
          }
