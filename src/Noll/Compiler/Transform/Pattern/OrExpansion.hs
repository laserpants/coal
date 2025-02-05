{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Pattern.OrExpansion (
  OrPattern (..),
  expandExpressionOrPatterns,
) where

import Data.Semigroup (sconcat)
import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Compiler.Transform.Expression (overExpression, mapMOverExpression)
import Noll.Language (
  Clause (..),
  Module (..),
  Expression (..),
  Definition (..),
  Pattern (..),
  Function (..),
  Type (..),
 )

import qualified Noll.Common.List1 as List1

--baz :: (Monad m) => Module a k t -> m (Module a k t) 
--baz = 

baz :: (Monad m) => Definition a k t -> m (Definition a k t) 
baz = 
  \case
      DAnnotation u d ->
        DAnnotation u <$> baz d
      DFunction name f ->
        undefined -- DFunction name <$> foo f
      DConstant name g ->
        undefined
      d ->
        pure d

--foo :: (Monad m) => Function e a t -> m (Function e a t) 
--foo = overExpression expandExpressionOrPatterns

expandExpressionOrPatterns :: (Monad m) => Expression a t -> m (Expression a t)
expandExpressionOrPatterns = mapMOverExpression go
 where
  go =
    \case
      EMatch a t e cs -> do
        cs1 <- sconcat <$> traverse expandOrPatterns cs
        pure (EMatch a t e cs1)
      EFold a t es cs e -> do
        cs1 <- sconcat <$> traverse expandOrPatterns cs
        pure (EFold a t es cs1 e)
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
      PRecord{} ->
        error "TODO"
      PListCons{} ->
        error "TODO"
      PListLiteral{} ->
        error "TODO"
      PShorthand{} ->
        error "TODO"
      PAtVariable{} ->
        error "TODO"
