{-# LANGUAGE OverloadedStrings #-}

module Extras.Operators (
  (&&.),
  (||.),
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

{-# INLINE (<.>) #-}
(<.>) :: Text -> Text -> Text
lhs <.> rhs = lhs <> "." <> rhs
