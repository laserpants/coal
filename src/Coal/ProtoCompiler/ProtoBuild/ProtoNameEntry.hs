{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry (
  ProtoDataConstructorEntry (..),
  ProtoNameEntry (..),
  ProtoHasName (..),
) where

import Coal.Language
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Import (Import (..))
import Data.Binary (Binary)
import Extras (Name, Set)
import GHC.Generics (Generic)

type IndexedConstructor = DataConstructor TypeIndex Kind IndexedType

data ProtoDataConstructorEntry a = ProtoDataConstructorEntry
  { protoOdataConstructorEntryMetaData :: a
  , protoOdataConstructorEntryName :: Name
  , protoOdataConstructorEntryConstructor :: IndexedConstructor
  , protoOdataConstructorEntryConstructorSet :: Set Name
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance (Binary a) => Binary (ProtoDataConstructorEntry a)

data ProtoNameEntry
  = ProtoNName Name IndexedScheme
  | ProtoNType Name Kind
  | ProtoNTrait Name
  | ProtoNTypeAlias Name
  | PRotoNPlaceholder Name
  deriving (Show, Eq, Ord, Read, Generic)

instance Binary ProtoNameEntry

class ProtoHasName a where
  protoOnameOf :: a -> Name

instance ProtoHasName ProtoNameEntry where
  protoOnameOf =
    \case
      ProtoNName name _ ->
        name
      ProtoNType name _ ->
        name
      ProtoNTrait name ->
        name
      ProtoNTypeAlias name ->
        name
      PRotoNPlaceholder name ->
        name

instance ProtoHasName (Import a) where
  protoOnameOf =
    \case
      NameImport _ name ->
        name
      TypeImport _ name _ ->
        name

instance ProtoHasName (Export a) where
  protoOnameOf =
    \case
      NameExport _ name ->
        name
      TypeExport _ name _ ->
        name

instance ProtoHasName (ProtoDataConstructorEntry a) where
  protoOnameOf =
    protoOdataConstructorEntryName
