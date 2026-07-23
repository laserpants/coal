{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Type extraction and manipulation.

Provides the 'HasType' type class for extracting types from typed AST nodes,
plus utilities for working with function types:

  * 'typeOf': Extract the type from an expression, label, operator, or
    primitive
  * 'foldType': Construct a function type from a result and a list of argument
    types
  * 'unfoldType': Decompose a function type into a list of argument types plus
    the result type
  * 'returnTypeOf': Extract the final result type from a (possibly nested)
    function type

Instances are provided for 'Type', 'Prim', 'Op', 'Expr', and 'Label'.
-}
module Coal.Kernel.Language.Type.HasType (
  HasType (typeOf),
  foldType,
  unfoldType,
  returnTypeOf,
) where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NonEmpty

import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.Constructors (arrow)
import qualified Coal.Kernel.Language.Type.Constructors as Type
import Coal.Kernel.Language.Type.Row (extend)

class HasType t where
  typeOf :: t -> Type

instance HasType Type where
  typeOf = id

instance HasType Prim where
  typeOf =
    \case
      PUnit{} ->
        Type.unit
      PBool{} ->
        Type.bool
      PInt32{} ->
        Type.int32
      PInt64{} ->
        Type.int64
      PBignum{} ->
        Type.bignum
      PFloat{} ->
        Type.float
      PDouble{} ->
        Type.double
      PChar{} ->
        Type.char
      PString{} ->
        Type.string

instance (HasType t) => HasType (Label t) where
  typeOf (Label t _) = typeOf t

instance HasType (Op a) where
  typeOf =
    \case
      OLtInt32{} ->
        Type.bool
      OLtInt64{} ->
        Type.bool
      OLtFloat{} ->
        Type.bool
      OLtDouble{} ->
        Type.bool
      OLteInt32{} ->
        Type.bool
      OLteInt64{} ->
        Type.bool
      OLteFloat{} ->
        Type.bool
      OLteDouble{} ->
        Type.bool
      OGtInt32{} ->
        Type.bool
      OGtInt64{} ->
        Type.bool
      OGtFloat{} ->
        Type.bool
      OGtDouble{} ->
        Type.bool
      OGteInt32{} ->
        Type.bool
      OGteInt64{} ->
        Type.bool
      OGteFloat{} ->
        Type.bool
      OGteDouble{} ->
        Type.bool
      OEqInt32{} ->
        Type.bool
      OEqInt64{} ->
        Type.bool
      OEqFloat{} ->
        Type.bool
      OEqDouble{} ->
        Type.bool
      ONeInt32{} ->
        Type.bool
      ONeInt64{} ->
        Type.bool
      ONeFloat{} ->
        Type.bool
      ONeDouble{} ->
        Type.bool
      OAnd{} ->
        Type.bool
      OOr{} ->
        Type.bool
      ONot{} ->
        Type.bool
      OAddInt32{} ->
        Type.int32
      OAddInt64{} ->
        Type.int64
      OAddFloat{} ->
        Type.float
      OAddDouble{} ->
        Type.double
      OSubInt32{} ->
        Type.int32
      OSubInt64{} ->
        Type.int64
      OSubFloat{} ->
        Type.float
      OSubDouble{} ->
        Type.double
      OMulInt32{} ->
        Type.int32
      OMulInt64{} ->
        Type.int64
      OMulFloat{} ->
        Type.float
      OMulDouble{} ->
        Type.double
      ODivInt32{} ->
        Type.int32
      ODivInt64{} ->
        Type.int64
      ODivFloat{} ->
        Type.float
      ODivDouble{} ->
        Type.double
      OEqChar{} ->
        Type.bool
      OEqBool{} ->
        Type.bool
      ONeChar{} ->
        Type.bool
      ONeBool{} ->
        Type.bool
      ONegInt32{} ->
        Type.int32
      ONegInt64{} ->
        Type.int64
      ONegFloat{} ->
        Type.float
      ONegDouble{} ->
        Type.double

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

{-# INLINE returnTypeOf #-}
returnTypeOf :: (HasType t) => t -> Type
returnTypeOf = NonEmpty.last . unfoldType . typeOf

instance (HasType t) => HasType (Expr t) where
  typeOf =
    \case
      EVar t ->
        typeOf t
      ECon t ->
        typeOf t
      ELet _ t ->
        typeOf t
      ELit t ->
        typeOf t
      ELam ts t ->
        foldType (typeOf t) (typeOf <$> ts)
      EApp t _ _ ->
        typeOf t
      EIf _ _ t ->
        typeOf t
      EOp op ->
        typeOf op
      ECase t _ _ ->
        typeOf t
      EExt f t1 t2 ->
        extend f (typeOf t1) (typeOf t2)
      ENil ->
        RNil
      EGet t _ ->
        typeOf t
      ECall _ _ k ->
        returnTypeOf k
