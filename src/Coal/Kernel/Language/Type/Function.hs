{- |
Function type utilities.

Provides helper functions for working with function types:

  * '(~>)': Infix constructor for function types
  * 'arity': Count the number of arguments in a function type
  * 'isFunction': Test whether a type is a function type
  * 'functionType': Construct a function type from argument and result types

Function types are represented as nested applications of the arrow type
constructor. This module provides a uniform interface for manipulating them.
-}
module Coal.Kernel.Language.Type.Function (
  (~>),
  arity,
  isFunction,
  functionType,
) where

import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.Constructors (arrow)
import Coal.Kernel.Language.Type.HasType (HasType (..), foldType, unfoldType)

{-# INLINE (~>) #-}
(~>) :: Type -> Type -> Type
(~>) = arrow

infixr 1 ~>

{-# INLINE arity #-}
arity :: Type -> Int
arity t = NonEmpty.length (unfoldType t) - 1

{-# INLINE isFunction #-}
isFunction :: (HasType t) => t -> Bool
isFunction e = arity (typeOf e) > 0

{-# INLINE functionType #-}
functionType :: (Functor f, Foldable f, HasType t, HasType u) => t -> f u -> Type
functionType a as = foldType (typeOf a) (typeOf <$> as)
