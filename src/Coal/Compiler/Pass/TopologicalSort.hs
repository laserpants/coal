{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TopologicalSort (passTopologicalSort) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Error (CompilerError (..), ErrorLocation (..))
import Coal.Compiler.Journal
import Coal.Compiler.Pass
import Coal.Compiler.Stack (CompilerFailureMode (..), CompilerT)
import Coal.Language
import Coal.Language.Module
import Control.Monad.Except
import Data.Graph (graphFromEdges, reverseTopSort)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (fst3)
import Extra (Name)

passTopologicalSort :: (Monad m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
passTopologicalSort =
  Pass
    { passName = "TopologicalSort"
    , runPass = pass
    }

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT Metadata m [Module Metadata Kind ()]
pass modules = do
  edges <- traverse (collectEdges s) modules
  let (graph, vertexToNode, _) = graphFromEdges edges
  pure (fst3 . vertexToNode <$> reverseTopSort graph)
 where
  s = Set.fromList (modulePathName <$> modules)

collectEdges :: (Monad m) => Set Name -> Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind (), Int, [Int])
collectEdges s m = do
  ks <- forM deps $
    \(loc, dep) ->
      case index dep of
        Just i ->
          pure [i]
        Nothing -> do
          tellErrors [ModuleNotFound dep (ErrorLocation name loc)]
          pure []
  if length ks == length deps
    then pure (m, k, concat ks)
    else throwError PreflightFailure
 where
  name = modulePathName m
  k = fromJust (index name)
  deps = dependencies m

  index :: Name -> Maybe Int
  index n = Set.lookupIndex n s

dependencies :: Module Metadata Kind () -> [(Metadata, Name)]
dependencies (Module _ _ ds) = concatMap importPath ds

importPath :: Definition Metadata Kind () -> [(Metadata, Name)]
importPath =
  \case
    DImport loc p _ ->
      [(loc, principalPath p)]
    _ ->
      []
