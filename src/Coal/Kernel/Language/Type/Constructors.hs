{-# LANGUAGE OverloadedStrings #-}

{- |
Type constructor smart constructors.

Provides named constructors for built-in types:

  * Primitive types: 'unit', 'bool', 'int32', 'int64', 'bignum', 'float',
    'double', 'char', 'string'
  * Function types: 'arrow'

All functions are inlined for zero-cost abstraction over raw 'TCon'
constructors.
-}
module Coal.Kernel.Language.Type.Constructors (
  tycon0,
  tycon1,
  tycon2,
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
) where

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Type (Type (..))

{-# INLINE tycon0 #-}
tycon0 :: Name -> Type
tycon0 con = TCon con []

{-# INLINE tycon1 #-}
tycon1 :: Name -> Type -> Type
tycon1 con t1 = TCon con [t1]

{-# INLINE tycon2 #-}
tycon2 :: Name -> Type -> Type -> Type
tycon2 con t1 t2 = TCon con [t1, t2]

{-# INLINE opaque #-}
opaque :: Type
opaque = TOpq

{-# INLINE unit #-}
unit :: Type
unit = tycon0 "unit"

{-# INLINE int32 #-}
int32 :: Type
int32 = tycon0 "int32"

{-# INLINE int64 #-}
int64 :: Type
int64 = tycon0 "int64"

{-# INLINE bignum #-}
bignum :: Type
bignum = tycon0 "bignum"

{-# INLINE bool #-}
bool :: Type
bool = tycon0 "bool"

{-# INLINE float #-}
float :: Type
float = tycon0 "float"

{-# INLINE double #-}
double :: Type
double = tycon0 "double"

{-# INLINE char #-}
char :: Type
char = tycon0 "char"

{-# INLINE string #-}
string :: Type
string = tycon0 "string"

{-# INLINE arrow #-}
arrow :: Type -> Type -> Type
arrow = tycon2 "/"

infixr 1 `arrow`
