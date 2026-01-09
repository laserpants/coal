{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module (
  Module (..),
  Export (..),
  overModuleDefinitions,
  overModuleDefinitionsM,
  fromDefinitionList,
  modulePathName,
  principalPath,
  qualified,
  importedPaths,
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
  module Coal.Language.Module.Import,
  module Coal.Language.Module.Export,
) where

import Coal.Language.Module.Definition (Definition (..), Path (..), definitionName, importPath)
import Coal.Language.Module.Definition.Alias
import Coal.Language.Module.Definition.Constant
import Coal.Language.Module.Definition.Cotype
import Coal.Language.Module.Definition.Fold
import Coal.Language.Module.Definition.Function
import Coal.Language.Module.Definition.Instance
import Coal.Language.Module.Definition.Trait
import Coal.Language.Module.Definition.Type
import Coal.Language.Module.Definition.Unfold
import Coal.Language.Module.Export
import Coal.Language.Module.Import
import Coal.Language.Module.Path (principalPath)
import Data.Data (Data, Typeable)
import Data.Maybe (mapMaybe)
import Extras (Name, Over)

data Module a k t = Module
  { modulePath :: Path
  , moduleExports :: [Export a]
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
fromDefinitionList :: Path -> [Export a] -> [Definition a k t] -> Module a k t
fromDefinitionList path exports = foldr insertDefinition (Module path exports mempty)

{-# INLINE modulePathName #-}
modulePathName :: Module a k t -> Name
modulePathName = principalPath . modulePath

qualified :: Name -> Path -> Name
qualified name path = principalPath path <> "." <> name

importedPaths :: Module a k t -> [(a, Path)]
importedPaths = mapMaybe importPath . moduleDefinitions
