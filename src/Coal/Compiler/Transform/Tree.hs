{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Tree (
  replace,
  replaceWith,
  replaceMultipleWith,
  rename,
) where

import Coal.Common.FreeVars (BoundVars (..))
import Coal.Common.Label (Label (..))
import Coal.Language
import Control.Monad.Identity (runIdentity)
import Data.Data (Data)
import Extras (Name, const2, (<$$>))

class TreeTransform e where
  transform :: (Monad m, Data a, Data t, Ord t) => Name -> (a -> t -> m (Expression a t)) -> e a t -> m (e a t)

instance TreeTransform (Binding Expression) where
  transform name f =
    \case
      BPattern a p e ->
        BPattern a p <$> transform name f e
      BFunction a n ps e ->
        BFunction a name ps <$> transform n f e

instance TreeTransform (Guard Expression) where
  transform name f =
    \case
      CGuard e ->
        CGuard <$> transform name f e

instance TreeTransform (Choice Expression) where
  transform name f =
    \case
      CPlain a gs e ->
        CPlain a
          <$> traverse (transform name f) gs
          <*> transform name f e
      CLambda{} ->
        error "TODO"

instance TreeTransform Clause where
  transform name f =
    \case
      EClause a ps cs
        | name `isNotBoundIn` ps ->
            EClause a ps <$> traverse (transform name f) cs
        | otherwise ->
            pure (EClause a ps cs)

instance TreeTransform CompiledClause where
  transform name f =
    \case
      ECompiledClause loc lls e
        | name `isNotBoundIn` lls ->
            ECompiledClause loc lls <$> transform name f e
        | otherwise ->
            pure (ECompiledClause loc lls e)

instance TreeTransform Expression where
  transform name f =
    \case
      EAnnotation _ _ e ->
        transform name f e
      var@(EVariable a (Label t name1))
        | name == name1 ->
            f a t
        | otherwise ->
            pure var
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
      EFocus field ll1 ll2 e1 e2 -> do
        EFocus field ll1 ll2 <$> transform name f e1 <*> transform name f e2
      EIf a t e1 e2 e3 ->
        EIf a t
          <$> transform name f e1
          <*> transform name f e2
          <*> transform name f e3
      expr@ELiteral{} ->
        pure expr
      EConstructor a ll@(Label t name1)
        | name == name1 ->
            f a t
        | otherwise ->
            pure (EConstructor a ll)
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
      ELambdaMatch a t cs me ->
        ELambdaMatch a t
          <$> traverse (transform name f) cs
          <*> traverse (transform name f) me
      ECompiledMatch a t e cs ->
        ECompiledMatch a t
          <$> transform name f e
          <*> traverse (transform name f) cs
      EFold a t es cs me ->
        EFold a t
          <$> traverse (transform name f) es
          <*> traverse (transform name f) cs
          <*> traverse (transform name f) me
      ECodataSelect a ll e1 e2 ->
        ECodataSelect a ll
          <$> traverse (transform name f) e1
          <*> traverse (transform name f) e2
      expr@EUnaryOperator{} ->
        pure expr
      expr@EBinaryOperator{} ->
        pure expr
      EListLiteral a t es ->
        EListLiteral a t <$> traverse (transform name f) es
      ETuple a t es ->
        ETuple a t <$> traverse (transform name f) es
      ECodataRecord a t d ->
        ECodataRecord a t <$> traverse (transform name f) d
      _ ->
        error "TODO"

{-# INLINE isNotBoundIn #-}
isNotBoundIn :: (BoundVars b) => Name -> b -> Bool
isNotBoundIn name obj = name `notElem` boundIn obj

replace :: (Ord t, Data a, Data t) => Name -> (a -> t -> Expression a t) -> Expression a t -> Expression a t
replace name f = runIdentity . transform name (pure <$$> f)

replaceWith :: (Ord t, Data a, Data t) => Name -> Expression a t -> Expression a t -> Expression a t
replaceWith name = replace name . const2

replaceMultipleWith :: (Ord t, Data a, Data t) => [(Name, Expression a t)] -> Expression a t -> Expression a t
replaceMultipleWith = flip $ foldr (uncurry replaceWith)

rename :: (Ord t, Data a, Data t) => Name -> Name -> Expression a t -> Expression a t
rename old name = replace old var where var a t = EVariable a (Label t name)
