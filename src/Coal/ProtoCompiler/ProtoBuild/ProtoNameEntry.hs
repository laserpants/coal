{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry (ProtoNameEntry (..)) where

import Coal.Language
import Data.Binary (Binary)
import Extras (Name)
import GHC.Generics (Generic)

data ProtoNameEntry
  = ProtoNName Name IndexedScheme
  | ProtoNType Name IndexedScheme
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
      _ ->
        undefined
