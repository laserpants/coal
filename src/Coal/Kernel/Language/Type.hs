{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

{- |
Core type language.

Defines the type system for Coal kernel language, including:

  * Type constructors for built-in and user-defined types
  * Row polymorphism for extensible records
  * The opaque type; this type typically maps to an opaque pointer in IR

= Row types

Rows represent field collections in record types. They are built from:

  * 'RNil': the empty row
  * 'RExt': row extension with a labeled field

The type checker normalizes rows by sorting fields lexicographically, enabling
structural comparison of record types.
-}
module Coal.Kernel.Language.Type (Type (..)) where

import Coal.Common.Name (Name)
import Data.Binary (Binary)
import GHC.Generics (Generic)

{- | Core language types.

The type system distinguishes:

  * __Nominal types__: type constructors applied to arguments (@TCon "int32"
    []@, @TCon "list" [a]@)
  * __Opaque type__: @TOpq@ provides structural type safety as a bridge between
    the surface language type system and IR types, retaining essential typing
    information.
  * __Row types__: 'RExt' and 'RNil' for extensible records
-}
data Type
  = -- | Type constructor
    TCon Name [Type]
  | -- | Opaque type
    TOpq
  | -- | Row extension
    RExt Name Type Type
  | -- | Empty row
    RNil
  deriving (Show, Eq, Ord, Read, Generic)

instance Binary Type
