{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module (
  Module (..),
  overModuleObjects,
  fromObjectList,
) where

import Noll.Language.Module.Object (Object (..), Path (..))
import Noll.Utils (Name)

data Module a k t = Module Path [Name] [Object a k t]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

{-# INLINE overModuleObjects #-}
overModuleObjects :: ([Object a k t] -> [Object a k t]) -> Module a k t -> Module a k t
overModuleObjects fn (Module path names om) = Module path names (fn om)

{-# INLINE insertObject #-}
insertObject :: Object a k t -> Module a k t -> Module a k t
insertObject obj = overModuleObjects (obj :)

{-# INLINE fromObjectList #-}
fromObjectList :: Path -> [Name] -> [Object a k t] -> Module a k t
fromObjectList path names = foldr insertObject (Module path names mempty)
