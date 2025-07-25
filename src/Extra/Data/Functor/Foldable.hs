module Extra.Data.Functor.Foldable (
  embed1,
  embed2,
  embed3,
  embed4,
  embed5,
  module Data.Functor.Foldable,
) where

import Data.Functor.Foldable (Base, Corecursive, embed)

{-# INLINE embed1 #-}
embed1 :: (Corecursive t) => (t1 -> Base t t) -> t1 -> t
embed1 t a = embed (t a)

{-# INLINE embed2 #-}
embed2 :: (Corecursive t) => (t1 -> t2 -> Base t t) -> t1 -> t2 -> t
embed2 t a b = embed (t a b)

{-# INLINE embed3 #-}
embed3 :: (Corecursive t) => (t1 -> t2 -> t3 -> Base t t) -> t1 -> t2 -> t3 -> t
embed3 t a b c = embed (t a b c)

{-# INLINE embed4 #-}
embed4 ::
  (Corecursive t) =>
  (t1 -> t2 -> t3 -> t4 -> Base t t) ->
  t1 ->
  t2 ->
  t3 ->
  t4 ->
  t
embed4 t a b c d = embed (t a b c d)

{-# INLINE embed5 #-}
embed5 ::
  (Corecursive t) =>
  (t1 -> t2 -> t3 -> t4 -> t5 -> Base t t) ->
  t1 ->
  t2 ->
  t3 ->
  t4 ->
  t5 ->
  t
embed5 t a b c d e = embed (t a b c d e)
