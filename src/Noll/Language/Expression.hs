{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Expression where

import Noll.Library (Some)

-- TODO
data Annotation a = Annotation a
  deriving (Show, Eq, Ord, Read)

data Expression t
  = -- | Type-annotated expression
    Annotated (Annotation ()) (Expression t)
  | -- | Function application
    Application t (Expression t) (Some (Expression t))
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
