module Extra.Operators (
  (&&.),
  (||.),
) where

{-# INLINE (&&.) #-}
(&&.) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
f &&. g = h where h e = f e && g e

infixr 3 &&.

{-# INLINE (||.) #-}
(||.) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
f ||. g = h where h e = f e || g e

infixr 2 ||.
