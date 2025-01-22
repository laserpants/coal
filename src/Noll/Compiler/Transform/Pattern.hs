{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Noll.Compiler.Transform.Pattern (
  mapOverPattern,
  mapMOverPattern,
) where

import Control.Arrow ((<<<))
import Control.Monad ((<=<))
import Control.Monad.Identity (runIdentity)
import Data.Map.Strict (Map)
import Noll.Common.List1 (NonEmpty)
import Noll.Language (Pattern (..))

mapOverPattern :: (Pattern a t -> Pattern a t) -> Pattern a t -> Pattern a t
mapOverPattern f = runIdentity . overPattern (pure . f)

mapMOverPattern :: (Monad m) => (Pattern a t -> m (Pattern a t)) -> Pattern a t -> m (Pattern a t)
mapMOverPattern = overPattern

class PatternContext o p where
  overPattern :: (Monad m) => (o -> m o) -> p -> m p

instance PatternContext (Pattern a t) (Pattern a t) where
  overPattern f =
    f
      <=< \case
        PAnnotation a t p1 ->
          PAnnotation a t <$> overPattern f p1

--        PAs name p ->
--          PAs name <$> overPattern f p
--        PConstructor ll ps ->
--          PConstructor ll <$> overPattern f ps
--        PListCons t p1 p2 ->
--          PListCons t <$> overPattern f p1 <*> overPattern f p2
--        PListLiteral t ps ->
--          PListLiteral t <$> overPattern f ps
--        PRecord t d mp ->
--          PRecord t <$> overPattern f d <*> overPattern f mp
--        POr p1 p2 ->
--          POr <$> overPattern f p1 <*> overPattern f p2
--        p ->
--          pure p

instance (PatternContext d p) => PatternContext d [p] where
  overPattern = traverse . overPattern

instance (PatternContext d p) => PatternContext d (NonEmpty p) where
  overPattern = traverse . overPattern

instance (PatternContext d d) => PatternContext d (Map p d) where
  overPattern = traverse . overPattern

instance (PatternContext d d) => PatternContext d (Maybe d) where
  overPattern = traverse . overPattern
