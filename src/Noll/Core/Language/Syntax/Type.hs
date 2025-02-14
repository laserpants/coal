{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Language.Syntax.Type (
  (~>),
  Type (..),
  arrow,
  bool,
  char,
  double,
  float,
  foldType,
  int32,
  int64,
  string,
  unit,
) where

import Noll.Core.Language.Type (Type (..))
import Noll.Utils (Name)

{-# INLINE tcon0 #-}
tcon0 :: Name -> Type
tcon0 con = TCon con []

{-# INLINE tcon1 #-}
tcon1 :: Name -> Type -> Type
tcon1 con t1 = TCon con [t1]

{-# INLINE tcon2 #-}
tcon2 :: Name -> Type -> Type -> Type
tcon2 con t1 t2 = TCon con [t1, t2]

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
