{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Noll.Kernel.Language.Op (Op (..)) where

import Data.Data (Data, Typeable)
import Data.Eq.Deriving (deriveEq1)
import Data.Generics.Uniplate.Data (childrenBi)
import Lang.Common.FreeVars (FreeVars (..))
import Text.Show.Deriving (deriveShow1)

-- | Binary operators
data Op a
  = -- | Equality
    OEqInt32 a a
  | OEqInt64 a a
  | OEqFloat a a
  | OEqDouble a a
  | -- | Inequality
    ONeInt32 a a
  | ONeInt64 a a
  | ONeFloat a a
  | ONeDouble a a
  | -- | Less than
    OLtInt32 a a
  | OLtInt64 a a
  | OLtFloat a a
  | OLtDouble a a
  | -- | Greater than
    OGtInt32 a a
  | OGtInt64 a a
  | OGtFloat a a
  | OGtDouble a a
  | -- | Less than or equal to
    OLteInt32 a a
  | OLteInt64 a a
  | OLteFloat a a
  | OLteDouble a a
  | -- | Greater than or equal to
    OGteInt32 a a
  | OGteInt64 a a
  | OGteFloat a a
  | OGteDouble a a
  | -- | Addition
    OAddInt32 a a
  | OAddInt64 a a
  | OAddFloat a a
  | OAddDouble a a
  | -- | Subtraction
    OSubInt32 a a
  | OSubInt64 a a
  | OSubFloat a a
  | OSubDouble a a
  | -- | Multiplication
    OMulInt32 a a
  | OMulInt64 a a
  | OMulFloat a a
  | OMulDouble a a
  | -- | Division
    ODivInt32 a a
  | ODivInt64 a a
  | ODivFloat a a
  | ODivDouble a a
  | -- | Logical OR
    OOr a a
  | -- | Logical AND
    OAnd a a
  | -- | Logical NOT
    ONot a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

deriveShow1 ''Op
deriveEq1 ''Op

instance forall a t. (Data a, Ord t, FreeVars a t) => FreeVars (Op a) t where
  freeIn = freeIn . (childrenBi :: Op a -> [a])
