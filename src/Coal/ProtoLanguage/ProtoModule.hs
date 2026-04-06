-- +
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoLanguage.ProtoModule (
  ProtoModule (..),
  ModuleExportList (..),
) where

import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Path (Path)
import Coal.ProtoLanguage.ProtoDefinition (ProtoDefinition (..))
import Data.Data (Data, Typeable)

data ModuleExportList a = Exports [Export a] | ExportAll
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Data
    , Typeable
    )

data ProtoModule a k t = ProtoModule
  { protoOmodulePath :: Path
  , protoOmoduleExportList :: ModuleExportList a
  , protoOmoduleDefinitions :: [ProtoDefinition a k t]
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
