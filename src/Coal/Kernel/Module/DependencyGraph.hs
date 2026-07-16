{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Module.DependencyGraph (
  topoSortModules,
  checkImportsSatisfied,
) where

import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (maximumBy)
import Data.Maybe (isNothing)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Module (Module (..))

{- | Given a set of known module names, find which module a qualified import
belongs to. Uses longest-prefix matching so that @"Utils.Function"@ is
preferred over @"Utils"@ when both are present and both are prefixes.

Handles both function imports (@"Data.List.head"@) and data constructor
imports (@"Ordering.LessThan"@) correctly, because it consults the actual
module Coal.set rather than relying on naming conventions.

Returns 'Nothing' if no known module is a prefix of the import.
-}
owningModule :: Set Name -> Name -> Maybe Name
owningModule available imp
  | Set.member imp available = Just imp
  | otherwise =
      case filter (\mname -> (mname <> ".") `Text.isPrefixOf` imp) (Set.toList available) of
        [] -> Nothing
        matches -> Just (maximumBy (comparing Text.length) matches)

{- | Sort modules in dependency order — imports before the modules that import
them. Uses Tarjan's algorithm via 'stronglyConnComp'.

Returns @Left names@ for the first dependency cycle detected, where @names@
are the module names that form the cycle. If multiple cycles exist only the
first is reported.
-}
topoSortModules :: [Module t] -> Either [Name] [Module t]
topoSortModules mods =
  let available = Set.fromList (map moduleName mods)
      edges m =
        Set.toList . Set.fromList $
          [ mname
          | imp <- moduleImports m
          , Just mname <- [owningModule available imp]
          ]
      sccs = stronglyConnComp [(m, moduleName m, edges m) | m <- mods]
   in traverse fromSCC sccs
 where
  fromSCC (AcyclicSCC m) = Right m
  fromSCC (CyclicSCC ms) = Left (map moduleName ms)

{- | Return all @(importer, missing-import)@ pairs for imports that are not
present in the supplied module list. An empty result means every import in the
set is satisfied by another module in the same set.

Useful for diagnosing "missing module" failures before invoking the code
generator.
-}
checkImportsSatisfied :: [Module t] -> [(Name, Name)]
checkImportsSatisfied mods =
  let available :: Set Name
      available = Set.fromList (map moduleName mods)
   in [ (moduleName m, imp)
      | m <- mods
      , imp <- moduleImports m
      , isNothing (owningModule available imp)
      ]
