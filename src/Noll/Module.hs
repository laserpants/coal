{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Module (
  Module (..),
  overModuleDefinitions,
  overModuleDefinitionsM,
  fromDefinitionList,
  module Noll.Module.Definition,
  module Noll.Module.Function,
  module Noll.Module.Constant,
) where

import Data.Data (Data, Typeable)
import Lang.Utils (Name, Over)
import Noll.Module.Constant
import Noll.Module.Definition (Definition (..), Path (..), definitionName)
import Noll.Module.Function

data Module a k t = Module Path [Name] [Definition a k t]
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

{-# INLINE overModuleDefinitions #-}
overModuleDefinitions :: Over (Module a k t) [Definition a k t]
overModuleDefinitions fn (Module path names defs) = Module path names (fn defs)

{-# INLINE overModuleDefinitionsM #-}
overModuleDefinitionsM :: (Monad m) => ([Definition a k t] -> m [Definition a k t]) -> Module a k t -> m (Module a k t)
overModuleDefinitionsM fn (Module path names defs) = Module path names <$> fn defs

{-# INLINE insertDefinition #-}
insertDefinition :: Definition a k t -> Module a k t -> Module a k t
insertDefinition def = overModuleDefinitions (def :)

{-# INLINE fromDefinitionList #-}
fromDefinitionList :: Path -> [Name] -> [Definition a k t] -> Module a k t
fromDefinitionList path names = foldr insertDefinition (Module path names mempty)
