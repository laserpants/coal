{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module (
  Module (..),
  overModuleDefinitions,
  overModuleDefinitionsM,
  fromDefinitionList,
  modulePathName,
  module Coal.Language.Module.Definition,
  module Coal.Language.Module.Definition.Function,
  module Coal.Language.Module.Definition.Constant,
  module Coal.Language.Module.Definition.Fold,
  module Coal.Language.Module.Definition.Alias,
  module Coal.Language.Module.Definition.Unfold,
  module Coal.Language.Module.Definition.Trait,
  module Coal.Language.Module.Definition.Type,
  module Coal.Language.Module.Definition.Cotype,
  module Coal.Language.Module.Definition.Instance,
) where

import Coal.Language.Module.Definition (Definition (..), Path (..), definitionName)
import Coal.Language.Module.Definition.Alias
import Coal.Language.Module.Definition.Constant
import Coal.Language.Module.Definition.Cotype
import Coal.Language.Module.Definition.Fold
import Coal.Language.Module.Definition.Function
import Coal.Language.Module.Definition.Instance
import Coal.Language.Module.Definition.Trait
import Coal.Language.Module.Definition.Type
import Coal.Language.Module.Definition.Unfold
import Data.Data (Data, Typeable)
import Extra (Name, Over)
import qualified Data.Text as Text

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

{-# INLINE modulePath #-}
modulePath :: Module a k t -> Path
modulePath (Module path _ _) = path

{-# INLINE modulePathName #-}
modulePathName :: Module a k t -> Name
modulePathName module_ = Text.intercalate "." path where Path path = modulePath module_
