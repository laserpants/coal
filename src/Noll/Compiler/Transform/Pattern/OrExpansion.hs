{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.OrExpansion (
  OrPattern (..),
  expandExpressionOrPatterns,
  baz2,
) where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendBiM, descendM)
import Data.Semigroup (sconcat)
import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Language (
  Clause (..),
  Constant (..),
  Definition (..),
  Expression (..),
  Function (..),
  Module (..),
  Pattern (..),
 )

import qualified Noll.Common.List1 as List1

baz2 :: (Monad m, Data a, Data t, Ord k) => Module a k t -> m (Module a k t)
baz2 =
  \case
    Module path ns ds ->
      Module path ns <$> traverse baz ds

baz :: (Monad m, Data a, Data t, Ord k) => Definition a k t -> m (Definition a k t)
baz =
  \case
    DAnnotation u d ->
      DAnnotation u <$> baz d
    DFunction name f ->
      DFunction name <$> foo f
    DConstant name g ->
      DConstant name <$> bar g
    d ->
      pure d

foo :: (Monad m, Data a, Data t) => Function Expression a t -> m (Function Expression a t)
foo =
  \case
    Function a u ps e ->
      Function a u ps <$> expandExpressionOrPatterns e

bar :: (Monad m, Data a, Data t) => Constant Expression a t -> m (Constant Expression a t)
bar =
  \case
    Constant a u e ->
      Constant a u <$> expandExpressionOrPatterns e

expandExpressionOrPatterns :: (Monad m, Data a, Data t) => Expression a t -> m (Expression a t)
expandExpressionOrPatterns =
  descendM $
    \case
      EMatch a t e cs ->
        EMatch a t e . sconcat <$> traverse expandOrPatterns cs
      EFold a t es cs e ->
        EFold a t es . sconcat <$> traverse expandOrPatterns cs <*> pure e
      e ->
        pure e

class OrPattern a where
  expandOrPatterns :: (Monad m) => a -> m (List1 a)

instance OrPattern (Clause e a t) where
  expandOrPatterns =
    \case
      EClause a p cs -> do
        q1 :| qs <- expandOrPatterns p
        pure (EClause a q1 cs :| [EClause a q cs | q <- qs])

instance OrPattern (Pattern a t) where
  expandOrPatterns =
    \case
      PAnnotation a t p -> do
        q1 :| qs <- expandOrPatterns p
        pure (PAnnotation a t q1 :| [PAnnotation a t q | q <- qs])
      PConstructor a ll ps -> do
        qs1 :| qss <- sequence <$> traverse expandOrPatterns ps
        pure (PConstructor a ll qs1 :| [PConstructor a ll qs | qs <- qss])
      POr _ _ p1 p2 -> do
        qs1 <- expandOrPatterns p1
        qs2 <- expandOrPatterns p2
        pure (qs1 <> qs2)
      p@PAny{} ->
        pure (List1.singleton p)
      p@PVariable{} ->
        pure (List1.singleton p)
      p@PLiteral{} ->
        pure (List1.singleton p)
      p@PAtVariable{} ->
        pure (List1.singleton p)
      PRecord{} ->
        error "TODO"
      PListCons{} ->
        error "TODO"
      PListLiteral{} ->
        error "TODO"
      PShorthand{} ->
        error "TODO"
