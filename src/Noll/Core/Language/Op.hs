{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Noll.Core.Language.Op (Op (..)) where

import Data.Eq.Deriving (deriveEq1)
import Noll.AST.HasFree (HasFree (..))
import Text.Show.Deriving (deriveShow1)

-- | Binary operators
data Op a
  = -- | Equality
    OEqInt32 a a
  | OEqInt64 a a
  | -- | Inequality
    ONEqInt32 a a
  | ONEqInt64 a a
  | -- | Less than
    OLtInt32 a a
  | OLtInt64 a a
  | -- | Greater than
    OGtInt32 a a
  | OGtInt64 a a
  | -- | Less than or equal to
    OLtEInt32 a a
  | OLtEInt64 a a
  | -- | Greater than or equal to
    OGtEInt32 a a
  | OGtEInt64 a a
  | -- | Addition
    OAddInt32 a a
  | OAddInt64 a a
  | -- | Subtraction
    OSubInt32 a a
  | OSubInt64 a a
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
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

deriveShow1 ''Op
deriveEq1 ''Op

instance (Ord t, HasFree a t) => HasFree (Op a) t where
  freeIn =
    \case
      OEqInt32 e1 e2 ->
        freeIn e1 <> freeIn e2
      OEqInt64 e1 e2 ->
        freeIn e1 <> freeIn e2
      ONEqInt32 e1 e2 ->
        freeIn e1 <> freeIn e2
      ONEqInt64 e1 e2 ->
        freeIn e1 <> freeIn e2
      OLtInt32 e1 e2 ->
        freeIn e1 <> freeIn e2
      OLtInt64 e1 e2 ->
        freeIn e1 <> freeIn e2
      OGtInt32 e1 e2 ->
        freeIn e1 <> freeIn e2
      OGtInt64 e1 e2 ->
        freeIn e1 <> freeIn e2
      OLtEInt32 e1 e2 ->
        freeIn e1 <> freeIn e2
      OLtEInt64 e1 e2 ->
        freeIn e1 <> freeIn e2
      OGtEInt32 e1 e2 ->
        freeIn e1 <> freeIn e2
      OGtEInt64 e1 e2 ->
        freeIn e1 <> freeIn e2
      OAddInt32 e1 e2 ->
        freeIn e1 <> freeIn e2
      OAddInt64 e1 e2 ->
        freeIn e1 <> freeIn e2
      OSubInt32 e1 e2 ->
        freeIn e1 <> freeIn e2
      OSubInt64 e1 e2 ->
        freeIn e1 <> freeIn e2
      OMulInt32 e1 e2 ->
        freeIn e1 <> freeIn e2
      OMulInt64 e1 e2 ->
        freeIn e1 <> freeIn e2
      OMulFloat e1 e2 ->
        freeIn e1 <> freeIn e2
      OMulDouble e1 e2 ->
        freeIn e1 <> freeIn e2
      ODivInt32 e1 e2 ->
        freeIn e1 <> freeIn e2
      ODivInt64 e1 e2 ->
        freeIn e1 <> freeIn e2
      ODivFloat e1 e2 ->
        freeIn e1 <> freeIn e2
      ODivDouble e1 e2 ->
        freeIn e1 <> freeIn e2
      OOr e1 e2 ->
        freeIn e1 <> freeIn e2
      OAnd e1 e2 ->
        freeIn e1 <> freeIn e2
      ONot e ->
        freeIn e
