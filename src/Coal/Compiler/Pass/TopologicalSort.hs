{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TopologicalSort (passTopologicalSort) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module
import Data.Graph (graphFromEdges, reverseTopSort)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Tuple.Extra (fst3)
import Extra (Name)

passTopologicalSort :: (Monad m) => Pass a m [Module Metadata Kind ()] [Module Metadata Kind ()]
passTopologicalSort =
  Pass
    { passName = "TopologicalSort"
    , runPass = pass
    }

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT a m [Module Metadata Kind ()]
pass modules = pure (map (fst3 . vertexToNode) (reverseTopSort graph))
 where
  s = Set.fromList (modulePathName <$> modules)
  (graph, vertexToNode, _) =
    graphFromEdges
      [ (m, k, ks)
      | m <- modules
      , let k = index (modulePathName m)
      , let ks = index <$> dependencies m
      ]
  index :: Name -> Int
  index n = fromJust (Set.lookupIndex n s)

dependencies :: Module Metadata Kind () -> [Name]
dependencies (Module _ _ ds) = concatMap importPath ds

importPath :: Definition Metadata Kind () -> [Name]
importPath =
  \case
    DImport _ p _ ->
      [principalPath p]
    _ ->
      []
