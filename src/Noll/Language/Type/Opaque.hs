module Noll.Language.Type.Opaque (OpaqueType) where

import Noll.Language.Type (Type)
import Noll.Language.Type.Index (TypeIndex)

type OpaqueType = Type TypeIndex ()
