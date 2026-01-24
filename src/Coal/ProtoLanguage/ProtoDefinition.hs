{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoLanguage.ProtoDefinition where

import Data.Data (Data, Typeable)

data ProtoDefinition a
  = -- | Type definition
    ProtoDType a
  | -- | Type alias
    ProtoDTypeAlias a
  | -- | Function definition
    ProtoDFunction a
  | -- | Function
    ProtoDFunctionGroup a
  | -- | Top-level fold
    ProtoDFold a
  | -- | Top-level let-binding
    ProtoDLet a
  | -- | Import statement
    ProtoDImport a
  | -- | Namespace (qualified) import
    ProtoDQualifiedImport a
  | -- | Trait
    ProtoDTrait a
  | -- | Trait instance
    ProtoDInstance a
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
