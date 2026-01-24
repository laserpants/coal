{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoLanguage.ProtoModule (ProtoModule (..)) where

import Coal.Language.Module.Path (Path)
import Data.Data (Data, Typeable)

data ProtoModule = ProtoModule
  { protoOmodulePath :: Path
  --    , moduleExports :: [Export a]
  --    , moduleDefinitions :: [Definition a k t]
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , --    , Functor
      --    , Foldable
      --    , Traversable
      Data
    , Typeable
    )
