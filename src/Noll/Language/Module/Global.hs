{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Global (Global (..), globalExpressionRep) where

import Noll.Common.List1 (NonEmpty ((:|)))
import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Trait (Uses (..))
import Noll.Language.Type (IndexedType)
import Noll.Utils (Name)

data Global e a t = Global a (Uses t) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

globalExpressionRep :: Name -> Global Expression a IndexedType -> Expression a IndexedType
globalExpressionRep name (Global loc (Uses _ t) e) =
  ELet
    loc
    (BPattern loc (PVariable loc (Label t name)) e :| [])
    (EVariable loc (Label t name))
