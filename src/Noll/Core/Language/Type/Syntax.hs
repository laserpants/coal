{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Language.Type.Syntax (
  (~>),
  Type (..),
  arrow,
  bool,
  char,
  double,
  float,
  opaque,
  foldType,
  unfoldType,
  arity,
  int32,
  int64,
  string,
  unit,
  list,
  tuple,
  record,
) where

import Noll.Common.List1 (List1, (<|))
import Noll.Core.Language.Type (Type (..))
import Noll.Utils (Name)
import TextShow (showt)

import qualified Noll.Common.List1 as List1

{-# INLINE tcon0 #-}
tcon0 :: Name -> Type
tcon0 con = TCon con []

{-# INLINE tcon1 #-}
tcon1 :: Name -> Type -> Type
tcon1 con t1 = TCon con [t1]

{-# INLINE tcon2 #-}
tcon2 :: Name -> Type -> Type -> Type
tcon2 con t1 t2 = TCon con [t1, t2]

{-# INLINE opaque #-}
opaque :: Type
opaque = TOpq

{-# INLINE unit #-}
unit :: Type
unit = tcon0 "unit"

{-# INLINE int32 #-}
int32 :: Type
int32 = tcon0 "int32"

{-# INLINE int64 #-}
int64 :: Type
int64 = tcon0 "int64"

{-# INLINE bool #-}
bool :: Type
bool = tcon0 "bool"

{-# INLINE float #-}
float :: Type
float = tcon0 "float"

{-# INLINE double #-}
double :: Type
double = tcon0 "double"

{-# INLINE char #-}
char :: Type
char = tcon0 "char"

{-# INLINE string #-}
string :: Type
string = tcon0 "string"

{-# INLINE arrow #-}
arrow :: Type -> Type -> Type
arrow = tcon2 "->"

infixr 1 `arrow`

{-# INLINE (~>) #-}
(~>) :: Type -> Type -> Type
(~>) = arrow

infixr 1 ~>

{-# INLINE foldType #-}
foldType :: (Foldable f) => Type -> f Type -> Type
foldType = foldr arrow

{-# INLINE list #-}
list :: Type -> Type
list = tcon1 "list"

{-# INLINE tuple #-}
tuple :: Int -> [Type] -> Type
tuple n = TCon ("tuple" <> showt n)

{-# INLINE record #-}
record :: Type -> Type
record r = TCon "record" [r]

{-# INLINE arity #-}
arity :: Type -> Int
arity t = List1.length (unfoldType t) - 1

unfoldType :: Type -> List1 Type
unfoldType =
  \case
    TCon "->" [t1, t2] ->
      t1 <| unfoldType t2
    t ->
      List1.singleton t
