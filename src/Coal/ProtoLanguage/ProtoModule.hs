{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoLanguage.ProtoModule (ProtoModule (..)) where

import Coal.Language.Module.Path (Path)
import Coal.ProtoLanguage.ProtoDefinition (ProtoDefinition (..))
import Data.Data (Data, Typeable)

data ProtoModule a t = ProtoModule
  { protoOmodulePath :: Path
  , --    , moduleExports :: [Export a]
    protoOmoduleDefinitions :: [ProtoDefinition a t]
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
