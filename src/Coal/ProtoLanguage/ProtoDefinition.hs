{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoLanguage.ProtoDefinition where

import Data.Data (Data, Typeable)

data ProtoDefinition
  = -- | Type definition
    ProtoDType
  | -- | Type alias
    ProtoDTypeAlias
  | -- | Function definition
    ProtoDFunction
  | -- | Top-level fold
    ProtoDFold
  | -- | Top-level let-binding
    ProtoDLet
  | -- | Import statement
    ProtoDImport
  | -- | Namespace (qualified) import
    ProtoDQualifiedImport
  | -- | Trait
    ProtoDTrait
  | -- | Trait instance
    ProtoDInstance
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , --    Functor,
      --    Foldable,
      --    Traversable,
      Data
    , Typeable
    )
