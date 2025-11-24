{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.AST.Transform (replace, replaceWith, replaceMultipleWith, rename) where

import Coal.Common.FreeVars (BoundVars (..))
import Coal.Common.Label (Label (..))
import Coal.Language (Binding (..), Choice (..), Clause (..), CompiledClause (..), Expression (..), Guard (..))
import Control.Monad.Identity (runIdentity)
import Data.Data (Data)
import Extras (Name, const2, (<$$>))

class Transformable e where
  rewrite :: (Monad m, Data a, Data t, Ord t) => Name -> (a -> t -> m (Expression a t)) -> e a t -> m (e a t)

instance Transformable (Binding Expression) where
  rewrite name f =
    \case
      BPattern a p e ->
        BPattern a p <$> rewrite name f e
      BFunction a n ps e ->
        BFunction a name ps <$> rewrite n f e

instance Transformable (Guard Expression) where
  rewrite name f =
    \case
      CGuard e ->
        CGuard <$> rewrite name f e

instance Transformable (Choice Expression) where
  rewrite name f =
    \case
      CPlain a gs e ->
        CPlain a
          <$> traverse (rewrite name f) gs
          <*> rewrite name f e
      CLambda{} ->
        error "TODO"

instance Transformable Clause where
  rewrite name f =
    \case
      EClause a ps cs
        | name `isNotBoundIn` ps ->
            EClause a ps <$> traverse (rewrite name f) cs
        | otherwise ->
            pure (EClause a ps cs)

instance Transformable CompiledClause where
  rewrite name f =
    \case
      ECompiledClause loc lls e
        | name `isNotBoundIn` lls ->
            ECompiledClause loc lls <$> rewrite name f e
        | otherwise ->
            pure (ECompiledClause loc lls e)

instance Transformable Expression where
  rewrite name f =
    \case
      EAnnotation _ _ e ->
        rewrite name f e
      var@(EVariable a (Label t name1))
        | name == name1 ->
            f a t
        | otherwise ->
            pure var
      expr@(ELambda a ps e)
        | name `isNotBoundIn` ps ->
            ELambda a ps <$> rewrite name f e
        | otherwise ->
            pure expr
      ELet a gs e1 ->
        ELet a
          <$> traverse (rewrite name f) gs
          <*> ( if name `isNotBoundIn` gs
                  then rewrite name f e1
                  else pure e1
              )
      expr@(ERecursiveLet a p e1 e2) ->
        if name `isNotBoundIn` p
          then
            ERecursiveLet a p
              <$> rewrite name f e1
              <*> rewrite name f e2
          else pure expr
      ERecord a t d e ->
        ERecord a t
          <$> traverse (rewrite name f) d
          <*> traverse (rewrite name f) e
      ESelect a ll e ->
        ESelect a ll <$> rewrite name f e
      EFocus field ll1 ll2 e1 e2 -> do
        EFocus field ll1 ll2 <$> rewrite name f e1 <*> rewrite name f e2
      EIf a t e1 e2 e3 ->
        EIf a t
          <$> rewrite name f e1
          <*> rewrite name f e2
          <*> rewrite name f e3
      expr@ELiteral{} ->
        pure expr
      EConstructor a ll@(Label t name1)
        | name == name1 ->
            f a t
        | otherwise ->
            pure (EConstructor a ll)
      EApplication a t e1 es ->
        EApplication a t
          <$> rewrite name f e1
          <*> traverse (rewrite name f) es
      EListCons a t e1 e2 ->
        EListCons a t
          <$> rewrite name f e1
          <*> rewrite name f e2
      EMatch a t e cs ->
        EMatch a t
          <$> rewrite name f e
          <*> traverse (rewrite name f) cs
      ELambdaMatch a t cs me ->
        ELambdaMatch a t
          <$> traverse (rewrite name f) cs
          <*> traverse (rewrite name f) me
      ECompiledMatch a t e cs ->
        ECompiledMatch a t
          <$> rewrite name f e
          <*> traverse (rewrite name f) cs
      EFold a t es cs me ->
        EFold a t
          <$> traverse (rewrite name f) es
          <*> traverse (rewrite name f) cs
          <*> traverse (rewrite name f) me
      ECodataSelect a ll e1 e2 ->
        ECodataSelect a ll
          <$> traverse (rewrite name f) e1
          <*> traverse (rewrite name f) e2
      expr@EUnaryOperator{} ->
        pure expr
      expr@EBinaryOperator{} ->
        pure expr
      EListLiteral a t es ->
        EListLiteral a t <$> traverse (rewrite name f) es
      ETuple a t es ->
        ETuple a t <$> traverse (rewrite name f) es
      ECodataRecord a t d ->
        ECodataRecord a t <$> traverse (rewrite name f) d
      EFFICall a ll es e ->
        EFFICall a ll
          <$> traverse (rewrite name f) es
          <*> rewrite name f e
      _ ->
        error "TODO"

{-# INLINE isNotBoundIn #-}
isNotBoundIn :: (BoundVars b) => Name -> b -> Bool
isNotBoundIn name obj = name `notElem` boundIn obj

replace :: (Ord t, Data a, Data t) => Name -> (a -> t -> Expression a t) -> Expression a t -> Expression a t
replace name f = runIdentity . rewrite name (pure <$$> f)

replaceWith :: (Ord t, Data a, Data t) => Name -> Expression a t -> Expression a t -> Expression a t
replaceWith name = replace name . const2

replaceMultipleWith :: (Ord t, Data a, Data t) => [(Name, Expression a t)] -> Expression a t -> Expression a t
replaceMultipleWith = flip $ foldr (uncurry replaceWith)

rename :: (Ord t, Data a, Data t) => Name -> Name -> Expression a t -> Expression a t
rename old name = replace old var where var a t = EVariable a (Label t name)
