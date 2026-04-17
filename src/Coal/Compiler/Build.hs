{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Build
Description: Build state and environment management for the Coal compiler

This module defines the Build data structure, which serves as the central
repository for compiler state during compilation. It accumulates information
about types, names, instances, and dependencies as modules are processed.

The Build structure contains:
- Name environments for value and type bindings
- Type constructor and data constructor registries
- Trait and instance databases
- Qualified name mappings
- Kernel IR type information
- Module dependencies and compilation artifacts
-}
module Coal.Compiler.Build (
  -- * Types
  Build (..),
  InstanceMap,

  -- * Build construction
  emptyBuild,

  -- * Path and hash operations
  setBuildPath,
  setBuildBitcode,
  setBuildHash,
  insertHash,

  -- * Name entry operations
  overBuildNames,
  insertBuildNameEntry,
  removeBuildNamePlaceholder,
  replaceBuildNameEntry,

  -- * Export operations
  insertBuildExportedName,

  -- * Data constructor operations
  overBuildDataConstructors,
  insertBuildDataConstructor,

  -- * Type constructor operations
  insertBuildTypeConstructor,

  -- * Trait operations
  insertBuildTrait,

  -- * Instance operations
  insertBuildInstance,

  -- * Type alias operations
  insertBuildAlias,

  -- * Kernel IR operations
  setBuildKernelNames,
  setBuildKernelIRTypes,
  setBuildKernelConstructors,

  -- * Qualified names
  setQualifiedNames,

  -- * Utility functions
  extractTypeEnvironment,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Hash256 (Hash256 (..))
import Coal.Compiler.Build.NameEntry
import Coal.Kernel.LLVM.IRType (IRType)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language (IndexedScheme, IndexedType)
import Coal.Language.Module.Path (Path (..))
import Control.Monad.State (execState, modify)
import Crypto.Hash (hash)
import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.List (nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Extras (Name, Set, forM_)
import GHC.Generics (Generic)

-- -----------------------------------------------------------------------------

-- * Types

type InstanceMap a = Map IndexedType a

{- | Build state containing all accumulated compiler information.

The Build structure is the main accumulator for compiler state, storing
information collected during compilation phases:

- 'buildPath': Current module path being compiled
- 'buildNames': Environment mapping names to their entries (values, types, traits, etc.)
- 'buildExportedNames': Set of names exported from the module
- 'buildDataConstructors': Data constructor information
- 'buildTypeConstructors': Type constructor information (arities, kinds)
- 'buildTraits': Trait (typeclass) definitions
- 'buildInstances': Trait instance implementations, indexed by trait name and type
- 'buildAliases': Type alias definitions
- 'buildDependencies': List of module dependencies
- 'buildQualifiedNames': Mapping from unqualified to qualified names
- 'buildBitcode': LLVM bitcode output (if generated)
- 'buildHash': Source hash for incremental compilation
- 'buildKernelNames': Kernel IR type environment
- 'buildKernelIRTypes': LLVM IR type mappings
- 'buildKernelConstructors': Constructor tag mappings
-}
data Build a = Build
  { buildPath :: Path
  , buildNames :: Environment [NameEntry]
  , buildExportedNames :: Set Name
  , buildDataConstructors :: Environment (DataConstructorEntry a)
  , buildTypeConstructors :: Environment (TypeConstructorEntry a)
  , buildTraits :: Environment (TraitEntry a)
  , buildInstances :: Environment (InstanceMap (InstanceEntry a))
  , buildAliases :: Environment (AliasEntry a)
  , buildDependencies :: [Path]
  , buildQualifiedNames :: Environment Name
  , buildBitcode :: Maybe ByteString
  , buildHash :: Maybe Hash256
  , buildKernelNames :: Environment Kernel.Type
  , buildKernelIRTypes :: Environment IRType
  , buildKernelConstructors :: Environment Int
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance (Binary a) => Binary (Build a)

-- -----------------------------------------------------------------------------

-- * Build construction

-- | Create an empty Build with all fields initialized to default values
emptyBuild :: Build a
emptyBuild =
  Build
    { buildPath = Path []
    , buildNames = mempty
    , buildExportedNames = mempty
    , buildDataConstructors = mempty
    , buildTypeConstructors = mempty
    , buildTraits = mempty
    , buildInstances = mempty
    , buildAliases = mempty
    , buildDependencies = mempty
    , buildQualifiedNames = mempty
    , buildBitcode = Nothing
    , buildHash = Nothing
    , buildKernelNames = mempty
    , buildKernelIRTypes = mempty
    , buildKernelConstructors = mempty
    }

-- -----------------------------------------------------------------------------

-- * Path and hash operations

-- | Update the current module path
setBuildPath :: Path -> Build a -> Build a
setBuildPath newBuildPath Build{..} =
  Build
    { buildPath = newBuildPath
    , ..
    }

-- | Set the LLVM bitcode output
setBuildBitcode :: ByteString -> Build a -> Build a
setBuildBitcode newBuildBitcode Build{..} =
  Build
    { buildBitcode = Just newBuildBitcode
    , ..
    }

-- | Set the source hash for incremental compilation
setBuildHash :: Hash256 -> Build a -> Build a
setBuildHash newBuildHash Build{..} =
  Build
    { buildHash = Just newBuildHash
    , ..
    }

-- | Compute and set the source hash from source text
insertHash :: Text -> Build a -> Build a
insertHash source = setBuildHash (Hash256 (hash (Text.encodeUtf8 source)))

-- -----------------------------------------------------------------------------

-- * Name entry operations

-- | Apply a function to the name environment
overBuildNames :: (Environment [NameEntry] -> Environment [NameEntry]) -> Build a -> Build a
overBuildNames f Build{..} =
  Build
    { buildNames = f buildNames
    , ..
    }

{- | Check if two name entries refer to the same name.
Used for deduplication when inserting name entries. Two entries are considered
equal if they have the same constructor and the same name, regardless of their
associated data (schemes, types, etc.).
-}
isSameNameEntry :: NameEntry -> NameEntry -> Bool
isSameNameEntry a b =
  case (a, b) of
    (NName n1 _, NName n2 _)
      | n1 == n2 -> True
    (NType n1 _, NType n2 _)
      | n1 == n2 -> True
    (NTrait n1, NTrait n2)
      | n1 == n2 -> True
    (NTypeAlias n1, NTypeAlias n2)
      | n1 == n2 -> True
    (NPlaceholder n1, NPlaceholder n2)
      | n1 == n2 -> True
    (_, _) ->
      False

-- | Insert a name entry, deduplicating if an equivalent entry already exists
insertBuildNameEntry :: NameEntry -> Build a -> Build a
insertBuildNameEntry entry =
  overBuildNames (Environment.adjust (nubBy isSameNameEntry) name . Environment.insertWith (<>) name [entry])
 where
  name = nameOf entry

-- | Remove a placeholder entry for a given name
removeBuildNamePlaceholder :: Name -> Build a -> Build a
removeBuildNamePlaceholder name =
  overBuildNames (Environment.adjust (filter (/= NPlaceholder name)) name)

-- | Replace an existing name entry with a new one, removing any placeholder
replaceBuildNameEntry :: NameEntry -> Build a -> Build a
replaceBuildNameEntry entry =
  removeBuildNamePlaceholder name . insertBuildNameEntry entry
 where
  name = nameOf entry

-- -----------------------------------------------------------------------------

-- * Export operations

-- | Apply a function to the exported names set
overBuildExportedNames :: (Set Name -> Set Name) -> Build a -> Build a
overBuildExportedNames f Build{..} =
  Build
    { buildExportedNames = f buildExportedNames
    , ..
    }

-- | Mark a name as exported from the module
insertBuildExportedName :: Name -> Build a -> Build a
insertBuildExportedName name = overBuildExportedNames (Set.insert name)

-- -----------------------------------------------------------------------------

-- * Data constructor operations

-- | Apply a function to the data constructor environment
overBuildDataConstructors :: (Environment (DataConstructorEntry a) -> Environment (DataConstructorEntry a)) -> Build a -> Build a
overBuildDataConstructors f Build{..} =
  Build
    { buildDataConstructors = f buildDataConstructors
    , ..
    }

-- | Register a data constructor
insertBuildDataConstructor :: Name -> DataConstructorEntry a -> Build a -> Build a
insertBuildDataConstructor name = overBuildDataConstructors . Environment.insert name

-- -----------------------------------------------------------------------------

-- * Type constructor operations

-- | Apply a function to the type constructor environment
overBuildTypeConstructors :: (Environment (TypeConstructorEntry a) -> Environment (TypeConstructorEntry a)) -> Build a -> Build a
overBuildTypeConstructors f Build{..} =
  Build
    { buildTypeConstructors = f buildTypeConstructors
    , ..
    }

-- | Register a type constructor
insertBuildTypeConstructor :: Name -> TypeConstructorEntry a -> Build a -> Build a
insertBuildTypeConstructor name = overBuildTypeConstructors . Environment.insert name

-- -----------------------------------------------------------------------------

-- * Trait operations

-- | Apply a function to the trait environment
overBuildTraits :: (Environment (TraitEntry a) -> Environment (TraitEntry a)) -> Build a -> Build a
overBuildTraits f Build{..} =
  Build
    { buildTraits = f buildTraits
    , ..
    }

-- | Register a trait definition
insertBuildTrait :: Name -> TraitEntry a -> Build a -> Build a
insertBuildTrait name = overBuildTraits . Environment.insert name

-- -----------------------------------------------------------------------------

-- * Instance operations

-- | Apply a function to the instance environment
overBuildInstances :: (Environment (InstanceMap (InstanceEntry a)) -> Environment (InstanceMap (InstanceEntry a))) -> Build a -> Build a
overBuildInstances f Build{..} =
  Build
    { buildInstances = f buildInstances
    , ..
    }

{- | Register a trait instance implementation.
Instances are indexed by trait name and implementing type, allowing efficient
lookup during dictionary resolution.
-}
insertBuildInstance :: Name -> IndexedType -> InstanceEntry a -> Build a -> Build a
insertBuildInstance name t entry = overBuildInstances (Environment.alter (Just . f) name)
 where
  f =
    \case
      Nothing ->
        Map.singleton t entry
      Just m ->
        Map.insert t entry m

-- -----------------------------------------------------------------------------

-- * Type alias operations

-- | Apply a function to the type alias environment
overBuildAliases :: (Environment (AliasEntry a) -> Environment (AliasEntry a)) -> Build a -> Build a
overBuildAliases f Build{..} =
  Build
    { buildAliases = f buildAliases
    , ..
    }

-- | Register a type alias definition
insertBuildAlias :: Name -> AliasEntry a -> Build a -> Build a
insertBuildAlias name = overBuildAliases . Environment.insert name

-- -----------------------------------------------------------------------------

-- * Kernel IR operations

-- | Set the kernel IR type environment (used during kernel compilation)
setBuildKernelNames :: Environment Kernel.Type -> Build a -> Build a
setBuildKernelNames env Build{..} =
  Build
    { buildKernelNames = env
    , ..
    }

-- | Set the LLVM IR type mappings for the kernel
setBuildKernelIRTypes :: Environment IRType -> Build a -> Build a
setBuildKernelIRTypes env Build{..} =
  Build
    { buildKernelIRTypes = env
    , ..
    }

-- | Set the constructor tag mappings for data types
setBuildKernelConstructors :: Environment Int -> Build a -> Build a
setBuildKernelConstructors env Build{..} =
  Build
    { buildKernelConstructors = env
    , ..
    }

-- -----------------------------------------------------------------------------

-- * Qualified names

-- | Set the mapping from unqualified to qualified names
setQualifiedNames :: Environment Name -> Build a -> Build a
setQualifiedNames names Build{..} =
  Build
    { buildQualifiedNames = names
    , ..
    }

-- -----------------------------------------------------------------------------

-- * Utility functions

{- | Extract the type environment (value name to type scheme mappings).
Collects all value bindings (NName entries) from the name environment,
creating a mapping from names to their type schemes. This is used during
type checking and inference to look up the types of values.
-}
extractTypeEnvironment :: Build a -> Environment IndexedScheme
extractTypeEnvironment Build{..} =
  flip execState mempty $ do
    forM_ (concat $ Environment.elems buildNames) $
      \case
        NName name s ->
          modify (Environment.insert name s)
        _ ->
          pure ()
