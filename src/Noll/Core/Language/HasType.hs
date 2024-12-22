module Noll.Core.Language.HasType where

import Noll.Core.Language.Type (Type (..))

class HasCoreType t where
  coreTypeOf :: t -> Type

instance HasCoreType Type where
  coreTypeOf = id
