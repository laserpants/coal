{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoLanguage.ProtoDefinition (
  ProtoDefinition (..),
  ProtoFunctionDefinition (..),
) where

import Coal.Language
import Coal.Language.Module.Path (Path (..))
import Data.Data (Data, Typeable)
import Extras (Name)
import Coal.Language.Module.Import (Import (..))

data ProtoFunctionDefinition a t = ProtoFunctionDefinition
  { protoOfunctionDefinitionExpression :: Expression a t
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    )

data ProtoDefinition a t
  = -- | Type definition
    ProtoDType a Name
  | -- | Type alias
    ProtoDTypeAlias a Name
  | -- | Function definition
    ProtoDFunction a Name (ProtoFunctionDefinition a t)
  | -- | Function
    ProtoDFunctionGroup a Name
  | -- | Top-level fold
    ProtoDFold a Name
  | -- | Top-level let-binding
    ProtoDLet a Name
  | -- | Import statement
    ProtoDImport a Path [Import a]
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
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    )
