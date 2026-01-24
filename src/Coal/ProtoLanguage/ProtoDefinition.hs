{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoLanguage.ProtoDefinition (ProtoDefinition (..)) where

import Coal.Language.Module.Path (Path (..))
import Data.Data (Data, Typeable)
import Extras (Name)

data ProtoDefinition a
  = -- | Type definition
    ProtoDType a Name
  | -- | Type alias
    ProtoDTypeAlias a Name
  | -- | Function definition
    ProtoDFunction a Name
  | -- | Function
    ProtoDFunctionGroup a Name
  | -- | Top-level fold
    ProtoDFold a Name
  | -- | Top-level let-binding
    ProtoDLet a Name
  | -- | Import statement
    ProtoDImport a Path
  | -- | Namespace (qualified) import
    ProtoDQualifiedImport a Path
  | -- | Trait
    ProtoDTrait a Name
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
