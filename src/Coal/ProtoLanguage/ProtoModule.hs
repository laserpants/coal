{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoLanguage.ProtoModule (ProtoModule (..)) where

import Data.Data (Data, Typeable)
import Coal.Language.Module.Path (Path)

data ProtoModule
  = ProtoModule
    { modulePath :: Path
--    , moduleExports :: [Export a]
--    , moduleDefinitions :: [Definition a k t]
    }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
--    , Functor
--    , Foldable
--    , Traversable
    , Data
    , Typeable
    )

