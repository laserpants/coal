{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoLanguage.ProtoDefinition where

import Data.Data (Data, Typeable)

data ProtoDefinition 
  = ProtoDType
  | ProtoDTypeAlias
  | ProtoDFunction
  | ProtoDFold
  | ProtoDLet
  | ProtoDImport
  | ProtoDQualifiedImport
  | ProtoDTrait
  | ProtoDInstance
  deriving 
    (Show, 
    Eq, 
    Ord, 
    Read, 
--    Functor, 
--    Foldable, 
--    Traversable, 
    Data, 
    Typeable
    )
