{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Kind.Error (KindError (..)) where

import Coal.Language.Type.Kind (Kind (..))
import Extras (Name)

data KindError
  = ENoTypeConstructor Name
  | ENoTrait Name
  | {- | The two kinds could not be unified. They are the kinds as they stood
    after the substitutions discovered so far had been applied, so they may
    mention kind variables.
    -}
    ECannotUnifyKinds Kind Kind
  | EInfiniteKind
  deriving (Show, Eq, Ord, Read)
