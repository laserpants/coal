{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Index (TypeIndex (..)) where

data TypeIndex k = TypeIndex k Int
  deriving (Show, Eq, Ord, Read, Functor, Foldable)
