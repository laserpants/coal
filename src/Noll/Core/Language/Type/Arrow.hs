{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Language.Type.Arrow (
  (~>),
  foldType,
  unfoldType,
  arity,
) where

import Noll.Common.List1 (List1, (<|))
import Noll.Core.Language.Type (Type (..))
import Noll.Core.Language.Type.Syntax (arrow)

import qualified Noll.Common.List1 as List1

{-# INLINE (~>) #-}
(~>) :: Type -> Type -> Type
(~>) = arrow

infixr 1 ~>

{-# INLINE foldType #-}
foldType :: (Foldable f) => Type -> f Type -> Type
foldType = foldr arrow

unfoldType :: Type -> List1 Type
unfoldType =
  \case
    TCon "->" [t1, t2] ->
      t1 <| unfoldType t2
    t ->
      List1.singleton t

{-# INLINE arity #-}
arity :: Type -> Int
arity t = List1.length (unfoldType t) - 1
