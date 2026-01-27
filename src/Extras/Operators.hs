{-# LANGUAGE OverloadedStrings #-}

module Extras.Operators (
  (&&.),
  (||.),
  (<>^),
  (<.>),
) where

import Data.Text (Text)

{-# INLINE (&&.) #-}
(&&.) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
f &&. g = h where h e = f e && g e

infixr 3 &&.

{-# INLINE (||.) #-}
(||.) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
f ||. g = h where h e = f e || g e

infixr 2 ||.

{-# INLINE (<>^) #-}
(<>^) :: (Monad m, Semigroup a) => m a -> m a -> m a
f <>^ g = do
  x <- f
  y <- g
  pure (x <> y)

infixr 6 <>^

{-# INLINE (<.>) #-}
(<.>) :: Text -> Text -> Text
lhs <.> rhs = lhs <> "." <> rhs
