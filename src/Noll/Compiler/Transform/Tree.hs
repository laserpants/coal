{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Tree (
  TreeTransform (..),
  replace,
  replaceWith,
  replaceMultipleWith,
  rename,
) where

import Control.Monad.Identity (runIdentity)
import Data.Data (Data)
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  CompiledClause (..),
  Expression (..),
  Guard (..),
 )
import Noll.Language.HasFree (appearsFreeIn, isNotBoundIn)
import Noll.Utils (Name, const2, (<$$>))

class TreeTransform e t where
  transform :: (Monad m, Data a, Data t, Ord t) => Name -> (a -> t -> m (Expression a t)) -> e a t -> m (e a t)

instance TreeTransform (Binding Expression) t where
  transform name f =
    \case
      BPattern a p e ->
        BPattern a p <$> transform name f e
      BFunction a n ps e ->
        BFunction a name ps <$> transform n f e -- TODO

instance TreeTransform (Guard Expression) t where
  transform name f =
    \case
      CGuard e ->
        CGuard <$> transform name f e

instance TreeTransform (Choice Expression) t where
  transform name f =
    \case
      CPlain a gs e ->
        CPlain a
          <$> traverse (transform name f) gs
          <*> transform name f e
      CLambda{} ->
        error "TODO"

instance TreeTransform Clause t where
  transform name f =
    \case
      EClause a ps cs
        | name `isNotBoundIn` ps ->
            EClause a ps <$> traverse (transform name f) cs
        | otherwise ->
            pure (EClause a ps cs)

instance TreeTransform CompiledClause t where
  transform name f =
    \case
      ECompiledClause lls e
        | name `isNotBoundIn` lls ->
            ECompiledClause lls <$> transform name f e
        | otherwise ->
            pure (ECompiledClause lls e)
      ECompiledField{} ->
        error "TODO"

instance TreeTransform Expression t where
  transform name f =
    \case
      EVariable a ll@(Label t name1)
        | name == name1 ->
            f a t
        | otherwise ->
            pure (EVariable a ll)
      expr@(ELambda a ps e)
        | name `isNotBoundIn` ps ->
            ELambda a ps <$> transform name f e
        | otherwise ->
            pure expr
      ELet a gs e1 ->
        ELet a
          <$> traverse (transform name f) gs
          <*> ( if name `isNotBoundIn` gs
                  then transform name f e1
                  else pure e1
              )
      expr@(ERecursiveLet a p e1 e2) ->
        if name `isNotBoundIn` p
          then
            ERecursiveLet a p
              <$> transform name f e1
              <*> transform name f e2
          else pure expr
      ERecord a t d e ->
        ERecord a t
          <$> traverse (transform name f) d
          <*> traverse (transform name f) e
      ESelect a ll e ->
        ESelect a ll <$> transform name f e
      EIf a t e1 e2 e3 ->
        EIf a t
          <$> transform name f e1
          <*> transform name f e2
          <*> transform name f e3
      expr@ELiteral{} ->
        pure expr
      expr@EConstructor{} ->
        pure expr
      EApplication a t e1 es ->
        EApplication a t
          <$> transform name f e1
          <*> traverse (transform name f) es
      EListCons a t e1 e2 ->
        EListCons a t
          <$> transform name f e1
          <*> transform name f e2
      EMatch a t e cs ->
        EMatch a t
          <$> transform name f e
          <*> traverse (transform name f) cs
      ECompiledMatch a t e cs ->
        ECompiledMatch a t
          <$> transform name f e
          <*> traverse (transform name f) cs
      EFold{} ->
        error "EFold"
      expr@EUnaryOperator{} ->
        pure expr
      expr@EBinaryOperator{} ->
        pure expr
      EListLiteral a t es ->
        EListLiteral a t <$> traverse (transform name f) es

replace :: (Ord t, Data a, Data t) => Name -> (a -> t -> Expression a t) -> Expression a t -> Expression a t
replace name f = runIdentity . transform name (pure <$$> f)

replaceWith :: (Ord t, Data a, Data t) => Name -> Expression a t -> Expression a t -> Expression a t
replaceWith name = replace name . const2

replaceMultipleWith :: (Ord t, Data a, Data t) => [(Name, Expression a t)] -> Expression a t -> Expression a t
replaceMultipleWith = flip $ foldr (uncurry replaceWith)

rename :: (Ord t, Data a, Data t) => Name -> Name -> Expression a t -> Expression a t
rename old name = replace old var where var a t = EVariable a (Label t name)
