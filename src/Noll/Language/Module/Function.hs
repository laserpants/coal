{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Function where

import Noll.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.HasType (HasType (..))
import Noll.Language.Pattern (Pattern)
import Noll.Language.Trait (Uses (..))
import Noll.Language.Type (IndexedType, foldType)
import Noll.Lib.List1 (List1, NonEmpty ((:|)))
import Noll.Utils (Name)

data Function e a t = Function a (Uses t) (List1 (Pattern a t)) (e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

expressionRep :: Name -> Function Expression a IndexedType -> Expression a IndexedType
expressionRep name (Function loc (Uses _ t) ps e) =
  ELet
    loc
    (BFunction loc name ps e :| [])
    (EVariable loc (Label (foldType t (typeOf <$> ps)) name))
