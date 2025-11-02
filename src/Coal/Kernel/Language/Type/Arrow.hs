{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Language.Type.Arrow (
  (~>),
  foldType,
  unfoldType,
  arity,
  isFunction,
  returnTypeOf,
  functionTypeOf,
) where

import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.Syntax (arrow)
import Coal.Kernel.Language.Typed (Typed (..))
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NonEmpty

{-# INLINE (~>) #-}
(~>) :: Type -> Type -> Type
(~>) = arrow

infixr 1 ~>

{-# INLINE foldType #-}
foldType :: (Foldable f) => Type -> f Type -> Type
foldType = foldr arrow

unfoldType :: Type -> NonEmpty Type
unfoldType =
  \case
    TCon "/" [t1, t2] ->
      t1 <| unfoldType t2
    t ->
      NonEmpty.singleton t

{-# INLINE arity #-}
arity :: Type -> Int
arity t = NonEmpty.length (unfoldType t) - 1

{-# INLINE isFunction #-}
isFunction :: (Typed t) => t -> Bool
isFunction f = arity (typeOf f) > 0

{-# INLINE returnTypeOf #-}
returnTypeOf :: (Typed t) => t -> Type
returnTypeOf = NonEmpty.last . unfoldType . typeOf

{-# INLINE functionTypeOf #-}
functionTypeOf :: (Functor f, Foldable f, Typed t, Typed u) => t -> f u -> Type
functionTypeOf a as = foldType (typeOf a) (typeOf <$> as)
