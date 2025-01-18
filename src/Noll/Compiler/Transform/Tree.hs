{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Transform.Tree (
  TreeTransform (..),
  replace,
  replaceWith,
  replaceManyWith,
  rename,
) where

import Control.Monad.Identity (runIdentity)
import Noll.Label (Label (..))
import Noll.Language (Expression (..))
import Noll.Language.HasFree (appearsFreeIn)
import Noll.Utils (Name, const2, (<$$>))

class TreeTransform e t where
  transform :: (Monad m, Ord t) => Name -> (a -> t -> m (Expression a t)) -> e a t -> m (e a t)

instance TreeTransform Expression t where
  transform name f =
    \case
      EVariable a ll@(Label t name1)
        | name == name1 ->
            f a t
        | otherwise ->
            pure (EVariable a ll)
      expr@(ELambda a ps e)
        | name `appearsFreeIn` expr ->
            ELambda a ps <$> transform name f e
        | otherwise ->
            pure expr

replace :: (Ord t) => Name -> (a -> t -> Expression a t) -> Expression a t -> Expression a t
replace name f = runIdentity . transform name (pure <$$> f)

replaceWith :: (Ord t) => Name -> Expression a t -> Expression a t -> Expression a t
replaceWith name = replace name . const2

replaceManyWith :: (Ord t) => [(Name, Expression a t)] -> Expression a t -> Expression a t
replaceManyWith = flip $ foldr (uncurry replaceWith)

rename :: (Ord t) => Name -> Name -> Expression a t -> Expression a t
rename old name = replace old var where var a t = EVariable a (Label t name)
