{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.Language.Type.Syntax (
  Type (..),
  arrow,
  bool,
  char,
  double,
  float,
  opaque,
  int32,
  int64,
  bignum,
  string,
  unit,
  list,
  tuple,
  record,
) where

import Coal.LegacyKernel.Language.Type (Type (..))
import Extras (Name)
import TextShow (showt)

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

{-# INLINE bignum #-}
bignum :: Type
bignum = tcon0 "bignum"

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
arrow = tcon2 "/"

infixr 1 `arrow`

{-# INLINE list #-}
list :: Type -> Type
list = tcon1 "list"

{-# INLINE tuple #-}
tuple :: [Type] -> Type
tuple ts = TCon ("tuple" <> showt (length ts)) ts

{-# INLINE record #-}
record :: Type -> Type
record r = TCon "record" [r]
