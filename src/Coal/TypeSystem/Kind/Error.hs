{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Kind.Error (KindError (..)) where

import Extras (Name)

data KindError
  = ENoTypeConstructor Name
  | ENoTrait Name
  | ECannotUnifyKinds
  | EInfiniteKind
  deriving (Show, Eq, Ord, Read)
