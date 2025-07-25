{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Kernel.Language.Type.Arrow (
  (~>),
  foldType,
  unfoldType,
  arity,
  isFunction,
  returnTypeOf,
  functionTypeOf,
) where

import Noll.Common.List1 (List1, (<|))
import Noll.Kernel.Language.Type (Type (..))
import Noll.Kernel.Language.Type.Syntax (arrow)
import Noll.Kernel.Language.Typed (Typed (..))

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
    TCon "/" [t1, t2] ->
      t1 <| unfoldType t2
    t ->
      List1.singleton t

{-# INLINE arity #-}
arity :: Type -> Int
arity t = List1.length (unfoldType t) - 1

{-# INLINE isFunction #-}
isFunction :: (Typed t) => t -> Bool
isFunction f = arity (typeOf f) > 0

{-# INLINE returnTypeOf #-}
returnTypeOf :: (Typed t) => t -> Type
returnTypeOf = List1.last . unfoldType . typeOf

{-# INLINE functionTypeOf #-}
functionTypeOf :: (Functor f, Foldable f, Typed t, Typed u) => t -> f u -> Type
functionTypeOf a as = foldType (typeOf a) (typeOf <$> as)
