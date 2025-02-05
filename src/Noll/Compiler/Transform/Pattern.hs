{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Noll.Compiler.Transform.Pattern (
  mapOverPattern,
  mapMOverPattern,
  overPattern,
) where

import Control.Monad.Identity (runIdentity)
import Data.Map.Strict (Map)
import Noll.Common.List1 (NonEmpty)
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  CompiledClause (..),
  Expression (..),
  Guard (..),
  Pattern (..),
 )
import Noll.Language.Module.Constant (Constant (..))
import Noll.Language.Module.Definition (Definition (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Utils (Over)

mapOverPattern :: (PatternContext p p) => Over p p
mapOverPattern f = runIdentity . overPattern (pure . f)

mapMOverPattern :: (Monad m, PatternContext p p) => (p -> m p) -> p -> m p
mapMOverPattern = overPattern

class PatternContext o p where
  overPattern :: (Monad m) => (o -> m o) -> p -> m p

instance PatternContext (Pattern a t) (Pattern a t) where
  overPattern f =
    \case
      PAnnotation a t p1 ->
        PAnnotation a t <$> (overPattern f =<< f p1)
      PConstructor a ll ps ->
        PConstructor a ll <$> (overPattern f =<< traverse f ps)
      PListCons a t p1 p2 ->
        PListCons a t
          <$> (overPattern f =<< f p1)
          <*> (overPattern f =<< f p2)
      PListLiteral a t ps ->
        PListLiteral a t <$> (overPattern f =<< traverse f ps)
      PRecord a t d p ->
        PRecord a t
          <$> (overPattern f =<< traverse f d)
          <*> (overPattern f =<< traverse f p)
      POr a t p1 p2 ->
        POr a t
          <$> (overPattern f =<< f p1)
          <*> (overPattern f =<< f p2)
      p@PVariable{} ->
        pure p
      p@PAtVariable{} ->
        pure p
      PAny{} ->
        error "TODO"
      PLiteral{} ->
        error "TODO"
      PShorthand{} ->
        error "TODO"

instance (PatternContext d p) => PatternContext d [p] where
  overPattern = traverse . overPattern

instance (PatternContext d p) => PatternContext d (NonEmpty p) where
  overPattern = traverse . overPattern

instance (PatternContext d d) => PatternContext d (Map p d) where
  overPattern = traverse . overPattern

instance (PatternContext d d) => PatternContext d (Maybe d) where
  overPattern = traverse . overPattern

instance PatternContext (Pattern a t) (Binding Expression a t) where
  overPattern f =
    \case
      BPattern a p e ->
        BPattern a
          <$> (overPattern f =<< f p)
          <*> overPattern f e
      BFunction{} ->
        error "TODO"

instance PatternContext (Pattern a t) (Clause Expression a t) where
  overPattern f =
    \case
      EClause a p cs ->
        EClause a
          <$> (overPattern f =<< f p)
          <*> overPattern f cs

instance PatternContext (Pattern a t) (CompiledClause Expression a t) where
  overPattern f =
    \case
      ECompiledClause lls e -> do
        ECompiledClause lls <$> overPattern f e
      ECompiledField{} ->
        error "TODO"

instance PatternContext (Pattern a t) (Choice Expression a t) where
  overPattern f =
    \case
      CPlain a gs e ->
        CPlain a
          <$> overPattern f gs
          <*> overPattern f e
      CLambda a ps gs e ->
        CLambda a
          <$> (overPattern f =<< traverse f ps)
          <*> traverse (overPattern f) gs
          <*> overPattern f e

instance PatternContext (Pattern a t) (Guard Expression a t) where
  overPattern f =
    \case
      CGuard e ->
        CGuard <$> overPattern f e

instance PatternContext (Pattern a t) (Expression a t) where
  overPattern f =
    \case
      EAnnotation a t e1 -> do
        EAnnotation a t <$> overPattern f e1
      EApplication a t e1 es ->
        EApplication a t
          <$> overPattern f e1
          <*> overPattern f es
      EIf a t e1 e2 e3 ->
        EIf a t
          <$> overPattern f e1
          <*> overPattern f e2
          <*> overPattern f e3
      ELet a gs e ->
        ELet a
          <$> overPattern f gs
          <*> overPattern f e

instance PatternContext (Pattern a t) (Constant Expression a t) where
  overPattern f =
    \case
      Constant a u e ->
        Constant a u <$> overPattern f e

instance PatternContext (Pattern a t) (Function Expression a t) where
  overPattern f =
    \case
      Function a u ps e ->
        Function a u
          <$> (overPattern f =<< traverse f ps)
          <*> overPattern f e

instance PatternContext (Pattern a t) (Definition a k t) where
  overPattern h =
    \case
      DFunction name f ->
        DFunction name <$> overPattern h f
      DConstant name g ->
        DConstant name <$> overPattern h g
      d ->
        pure d
