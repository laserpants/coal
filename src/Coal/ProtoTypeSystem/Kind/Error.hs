{-# LANGUAGE StrictData #-}

module Coal.ProtoTypeSystem.Kind.Error (ProtoKindError (..)) where

import Extras (Name)

data ProtoKindError
  = ProtoENoTypeConstructor Name
  | ProtoENoTrait Name
  | ProtoECannotUnifyKinds
  | ProtoEInfiniteKind
  deriving (Show, Eq, Ord, Read)
