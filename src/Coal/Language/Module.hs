-- +
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module (
  Module (..),
  ModuleExportList (..),
) where

import Coal.Language.Definition (Definition (..))
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Path (Path)
import Data.Data (Data, Typeable)

data ModuleExportList a
  = Exports [Export a]
  | ExportAll
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Data
    , Typeable
    )

data Module a k t = Module
  { modulePath :: Path
  , moduleExportList :: ModuleExportList a
  , moduleDefinitions :: [Definition a k t]
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
