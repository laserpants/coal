{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Coal.Graphviz.Dot (Dot (..), generateDotSyntax) where

import Data.Functor.Foldable (cata)
import qualified Coal.Kernel.Language as Kernel
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (principalPath)
import Coal.Pretty (CoalPretty (..))
import Control.Monad.State (State, forM, forM_, modify, runState)
import Data.Foldable (foldrM)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Dictionary, Name)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import TextShow (showt)

data DotShape
  = RectangleShape
  | EllipseShape
  | ParallelogramShape
  | HexagonShape
  | FolderShape
  | TriangleShape
  | DiamondShape
  | HouseShape
  | NoteShape
  deriving (Show, Eq, Read)

data DotNode = DotNode
  { dotNodeId :: Int
  , dotNodeLabel :: Text
  , dotNodeName :: Maybe Text
  , dotNodeShape :: DotShape
  }
  deriving (Show, Eq, Read)

data DotEdge = DotEdge
  { dotEdgeFrom :: Int
  , dotEdgeTo :: Int
  , dotEdgeLabel :: Maybe Text
  }
  deriving (Show, Eq, Read)

data DotState = DotState
  { dotStateSupply :: Int
  , dotStateNodes :: [DotNode]
  , dotStateEdges :: [DotEdge]
  }
  deriving (Show, Eq, Read)

instance Supply DotState where
  updateSupply f DotState{..} = DotState{dotStateSupply = f dotStateSupply, ..}
  getSupply = dotStateSupply

insertNode :: DotNode -> DotState -> DotState
insertNode node DotState{..} =
  DotState
    { dotStateNodes = node : dotStateNodes
    , ..
    }

insertEdge :: Int -> Int -> Maybe Text -> DotState -> DotState
insertEdge from to label DotState{..} =
  DotState
    { dotStateEdges = DotEdge from to label : dotStateEdges
    , ..
    }

type DotGen = State DotState

freshId :: DotGen Int
freshId = supplied id

emitNode :: DotNode -> DotGen ()
emitNode = modify . insertNode

emitEdge :: (Dot a) => Int -> a -> DotGen ()
emitEdge dotId to = do
  toId <- toDot to
  modify (insertEdge dotId toId Nothing)

emitEdgeWithLabel :: (Dot a) => Text -> Int -> a -> DotGen ()
emitEdgeWithLabel label dotId to = do
  toId <- toDot to
  modify (insertEdge dotId toId (Just label))

emitEdges :: (Traversable f, Dot a) => Int -> f a -> DotGen ()
emitEdges from tos = do
  ids <- forM tos toDot
  forM_ ids $ emitEdge from

emitEdgesWithLabels :: (Dot a) => [Text] -> Int -> [a] -> DotGen ()
emitEdgesWithLabels labels from tos = do
  ids <- forM tos toDot
  forM_ (zip labels ids) $
    \(ll, to) ->
      emitEdgeWithLabel ll from to

emitNamedShape :: DotShape -> Maybe Text -> Text -> DotGen Int
emitNamedShape shape name label = do
  nid <- freshId
  emitNode (DotNode nid label name shape)
  return nid

emitShape :: DotShape -> Text -> DotGen Int
emitShape shape = emitNamedShape shape Nothing

class Dot a where
  toDot :: a -> DotGen Int

instance Dot () where
  toDot () = emitShape EllipseShape "()"

instance Dot Name where
  toDot = emitShape RectangleShape

instance Dot Int where
  toDot = pure

instance Dot (DotGen Int) where
  toDot = id

instance (Dot v) => Dot (Dictionary v) where
  toDot m = do
    dotId <- emitShape RectangleShape "Dictionary"
    forM_ (Map.toList m) $
      \(k, v) -> do
        id1 <- emitNamedShape RectangleShape (Just k) "Key"
        emitEdge dotId id1
        emitEdge id1 v
    return dotId

instance (Dot a) => Dot (Maybe a) where
  toDot =
    \case
      Nothing -> do
        emitShape RectangleShape "Nothing"
      Just d -> do
        dotId <- emitShape RectangleShape "Just"
        emitEdge dotId d
        return dotId

instance (Dot t) => Dot (Label t) where
  toDot =
    \case
      Label t name -> do
        (id1, _) <- withTypeInfo t $ emitNamedShape NoteShape (Just name) "Label"
        return id1

instance Dot (Import a) where
  toDot =
    \case
      NameImport _ name ->
        emitNamedShape RectangleShape (Just name) "NameImport"
      TypeImport _ name names -> do
        dotId <- emitNamedShape RectangleShape (Just name) "TypeImport"
        _ <- foldrM connectDots dotId names
        return dotId

instance Dot (Export a) where
  toDot =
    \case
      NameExport _ name ->
        emitNamedShape RectangleShape (Just name) "NameExport"
      TypeExport _ name names -> do
        dotId <- emitNamedShape RectangleShape (Just name) "TypeExport"
        _ <- foldrM connectDots dotId names
        return dotId

instance Dot (ExportList a) where
  toDot =
    \case
      Exports exports -> do
        dotId <- emitShape FolderShape "Exports"
        emitEdges dotId exports
        return dotId
      ExportAll ->
        emitShape FolderShape "ExportsAll"

instance (HasKind (Type Parameter k), CoalPretty k, Dot t, Dot k, Show k) => Dot (Module a k t) where
  toDot =
    \case
      Module{..} -> do
        dotId <- emitNamedShape RectangleShape (Just $ principalPath modulePath) "Module"
        emitEdge dotId moduleExportList
        emitEdges dotId moduleDefinitions
        return dotId

emitDefinition :: (Dot t) => Text -> Text -> t -> DotGen Int
emitDefinition name label def = do
  dotId <- emitNamedShape TriangleShape (Just name) label
  emitEdge dotId def
  return dotId

instance (HasKind (Type Parameter k), CoalPretty k, Dot t, Dot k, Show k) => Dot (Definition a k t) where
  toDot =
    \case
      DType _ name def ->
        emitDefinition name "DType" def
      DTypeAlias _ name def ->
        emitDefinition name "DTypeAlias" def
      DFunction _ name def ->
        emitDefinition name "DFunction" def
      DLet _ name def ->
        emitDefinition name "DLet" def
      DFunctionGroup _ name defs -> do
        dotId <- emitNamedShape TriangleShape (Just name) "DFunctionGroup"
        emitEdges dotId defs
        return dotId
      DFold _ name def -> do
        emitDefinition name "DFold" def
      DImport _ path imports -> do
        dotId <- emitNamedShape TriangleShape (Just $ principalPath path) "DImport"
        _ <- foldrM connectDots dotId imports
        return dotId
      DNamespaceImport _ path -> do
        emitNamedShape TriangleShape (Just $ principalPath path) "DQualifiedImport"
      DTrait _ name def ->
        emitDefinition name "DTrait" def
      DInstance _ def -> do
        dotId <- emitShape TriangleShape "DInstance"
        emitEdge dotId def
        return dotId

connectDots :: (Dot a) => a -> Int -> DotGen Int
connectDots a from = do
  dotId <- toDot a
  emitEdge from dotId
  return dotId

instance (HasKind (Type Parameter k), CoalPretty k, Dot k, Show k) => Dot (TypeDefinition a k t) where
  toDot =
    \case
      TypeDefinition{..} -> do
        dotId <- emitShape EllipseShape "TypeDefinition"
        emitEdges dotId typeDefinitionParameters
        emitEdges dotId typeDefinitionConstructors
        return dotId

annotation :: (Dot t) => t -> DotGen Int
annotation t = do
  dotId <- emitShape RectangleShape "Annotation"
  emitEdge dotId t
  return dotId

instance (HasKind (Type Parameter k), CoalPretty k, Dot t, Show k) => Dot (FunctionDefinition a k t) where
  toDot =
    \case
      FunctionDefinition{..} -> do
        dotId <- emitShape EllipseShape "FunctionDefinition"
        emitEdge dotId (annotation functionDefinitionAnnotation)
        emitEdge dotId functionDefinitionType
        emitEdgesWithLabels numberedList dotId (NonEmpty.toList functionDefinitionPatterns)
        emitEdge dotId functionDefinitionExpression
        return dotId

instance (HasKind (Type Parameter k), CoalPretty k, Dot t, Show k) => Dot (LetDefinition a k t) where
  toDot =
    \case
      LetDefinition{..} -> do
        dotId <- emitShape EllipseShape "LetDefinition"
        emitEdge dotId (annotation letDefinitionAnnotation)
        emitEdge dotId letDefinitionType
        emitEdge dotId letDefinitionExpression
        return dotId

instance (HasKind (Type Parameter k), CoalPretty k, Dot t, Show k) => Dot (FoldDefinition a k t) where
  toDot =
    \case
      FoldDefinition{..} -> do
        dotId <- emitShape EllipseShape "FoldDefinition"
        emitEdge dotId (annotation foldDefinitionAnnotation)
        emitEdges dotId foldDefinitionClauses
        return dotId

instance (HasKind (Type Parameter k), Dot k, CoalPretty k, Show k) => Dot (TraitDefinition a k) where
  toDot =
    \case
      TraitDefinition{..} -> do
        dotId <- emitNamedShape EllipseShape (Just traitDefinitionTraitName) "TraitDefinition"
        emitEdges dotId traitDefinitionConstraints
        emitEdge dotId traitDefinitionParameter
        forM_ traitDefinitionInterface $
          \TraitDefinitionInterfaceEntry{..} -> do
            id1 <- emitNamedShape EllipseShape (Just traitDefinitionInterfaceEntryName) "Member"
            emitEdge dotId id1
            emitEdge id1 traitDefinitionInterfaceEntryScheme
        return dotId

instance (HasKind (Type Parameter k), Dot k, CoalPretty k, Show k, Dot t) => Dot (InstanceDefinition a k t) where
  toDot =
    \case
      InstanceDefinition{..} -> do
        dotId <- emitNamedShape EllipseShape (Just instanceDefinitionTraitName) "InstanceDefinition"
        emitEdges dotId instanceDefinitionConstraints
        emitEdge dotId instanceDefinitionType
        emitEdges dotId instanceDefinitionImplementations
        return dotId

instance (HasKind (Type Parameter k), Dot k, CoalPretty k, Show k) => Dot (AliasDefinition a k) where
  toDot =
    \case
      AliasDefinition{..} -> do
        dotId <- emitShape EllipseShape "AliasDefinition"
        emitEdges dotId aliasDefinitionParameters
        emitEdge dotId aliasDefinitionType
        return dotId

instance (Dot t, Dot (o k)) => Dot (DataConstructor o k t) where
  toDot =
    \case
      DataConstructor{..} -> do
        dotId <- emitNamedShape EllipseShape (Just constructorName) "DataConstructor"
        emitEdge dotId constructorScheme
        return dotId

instance (Dot k) => Dot (Parameter k) where
  toDot =
    \case
      Parameter{..} -> do
        (id1, _) <- withTypeInfo parameterKind $ emitNamedShape RectangleShape (Just parameterName) "Parameter"
        return id1

instance (Dot t, Dot (o k)) => Dot (Scheme o k t) where
  toDot =
    \case
      Forall{..} -> do
        dotId <- emitShape RectangleShape "Scheme"
        emitEdges dotId (Set.toList schemeTypeVariables)
        emitEdges dotId (Set.toList schemeTraits)
        emitEdge dotId schemeTypeBody
        return dotId

instance (Dot t) => Dot (Trait t) where
  toDot =
    \case
      Trait{..} -> do
        dotId <- emitNamedShape HexagonShape (Just traitName) "Trait"
        emitEdge dotId traitType
        return dotId

instance (Dot t) => Dot (Qualified t) where
  toDot (With traits t) = do
    dotId <- emitShape HexagonShape "With"
    case traits of
      [] -> do
        toId <- emitShape HexagonShape "[]"
        emitEdge dotId toId
      _ -> emitEdges dotId traits
    emitEdge dotId t
    return dotId

instance (Show k, CoalPretty k, HasKind (Type TypeIndex k)) => Dot (Type TypeIndex k) where
  toDot t = do
    (id1, _) <- withTypeInfo (kindOf t) $ emitShape HexagonShape (prettyType t)
    return id1

instance (Show k, CoalPretty k, HasKind (Type Parameter k)) => Dot (Type Parameter k) where
  toDot t = do
    (id1, _) <- withTypeInfo (kindOf t) $ emitShape HexagonShape (prettyType t)
    return id1

instance Dot Kind where
  toDot k = do
    emitShape HexagonShape (Text.pack $ show k)

instance (Dot t, Dot (Type Parameter k), Show k, CoalPretty k) => Dot (Binding Expression a k t) where
  toDot =
    \case
      BPattern _ p e -> do
        dotId <- emitShape RectangleShape "BPattern"
        emitEdgeWithLabel "let" dotId p
        emitEdgeWithLabel "=" dotId e
        return dotId
      BFunction _ name ps e -> do
        dotId <- emitNamedShape RectangleShape (Just name) "BFunction"
        emitEdges dotId ps
        emitEdge dotId e
        return dotId

withTypeInfo :: (Dot t) => t -> DotGen Int -> DotGen (Int, Int)
withTypeInfo t e = do
  id1 <- toDot e
  id2 <- toDot t
  emitEdge id1 id2
  return (id1, id2)

instance Dot Primitive where
  toDot =
    \case
      LUnit ->
        emitShape RectangleShape "LUnit"
      LBool b ->
        emitNamedShape RectangleShape (Just $ showt b) "LBool"
      LInt32 int ->
        emitNamedShape RectangleShape (Just $ showt int) "LInt32"
      LInt64 int ->
        emitNamedShape RectangleShape (Just $ showt int) "LInt64"
      LBignum int ->
        emitNamedShape RectangleShape (Just $ showt int) "LBignum"
      LFloat float ->
        emitNamedShape RectangleShape (Just $ showt float) "LFloat"
      LDouble double ->
        emitNamedShape RectangleShape (Just $ showt double) "LDouble"
      LChar c ->
        emitNamedShape RectangleShape (Just $ showt c) "LChar"
      LString str ->
        emitNamedShape RectangleShape (Just $ showt str) "LString"

instance (Dot t, Dot (Type Parameter k), Show k, CoalPretty k) => Dot (Expression a k t) where
  toDot =
    \case
      EAnnotation _ t e -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EAnnotation"
        emitEdge id2 e
        return id1
      EApplication _ t e es -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EApplication"
        emitEdgeWithLabel "@f" id2 e
        emitEdgesWithLabels numberedList id2 (NonEmpty.toList es)
        return id1
      ELambda _ ps e -> do
        dotId <- emitShape HouseShape "ELambda"
        emitEdgesWithLabels numberedList dotId (NonEmpty.toList ps)
        emitEdge dotId e
        return dotId
      ELet _ bs e -> do
        dotId <- emitShape HouseShape "ELet"
        emitEdges dotId bs
        emitEdge dotId e
        return dotId
      ERecursiveLet _ p e1 e2 -> do
        dotId <- emitShape HouseShape "ERecursiveLet"
        emitEdge dotId p
        emitEdgeWithLabel "=" dotId e1
        emitEdgeWithLabel "in" dotId e2
        return dotId
      EVariable _ (Label t name) -> do
        (id1, _) <- withTypeInfo t $ emitNamedShape HouseShape (Just name) "EVariable"
        return id1
      EConstructor _ (Label t name) -> do
        (id1, _) <- withTypeInfo t $ emitNamedShape HouseShape (Just name) "EConstructor"
        return id1
      ELiteral _ p -> do
        dotId <- emitShape HouseShape "ELiteral"
        emitEdge dotId p
        return dotId
      EIf _ t e1 e2 e3 -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EIf"
        emitEdge id2 e1
        emitEdgeWithLabel "then" id2 e2
        emitEdgeWithLabel "else" id2 e3
        return id1
      EOperator _ t op -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EOperator"
        emitEdge id2 op
        return id1
      ERecord _ t d me -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "ERecord"
        emitEdge id2 d
        emitEdge id2 me
        return id1
      EListCons _ t e1 e2 -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EListCons"
        emitEdge id2 e1
        emitEdge id2 e2
        return id1
      EListLiteral _ t [] -> do
        (id1, id2) <- withTypeInfo t $ emitShape ParallelogramShape "EListLiteral"
        emitEdge id2 (emitShape HexagonShape "[]")
        return id1
      EListLiteral _ t es -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EListLiteral"
        emitEdges id2 es
        return id1
      ETuple _ t es -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "ETuple"
        emitEdges id2 es
        return id1
      EMatch _ t e cs -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EMatch"
        emitEdge id2 e
        emitEdgesWithLabels numberedList id2 (NonEmpty.toList cs)
        return id1
      ELambdaMatch _ t cs -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "ELambdaMatch"
        emitEdges id2 cs
        return id1
      ECompiledMatch _ t e cs -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "ECompiledMatch"
        emitEdge id2 e
        emitEdges id2 cs
        return id1
      EFold _ t es cs -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EFold"
        emitEdges id2 es
        emitEdges id2 cs
        return id1
      ESelect _ (Label t name) e -> do
        (id1, id2) <- withTypeInfo t $ emitNamedShape HouseShape (Just name) "ESelect"
        emitEdge id2 e
        return id1
      EFocus _ name ll1 ll2 e1 e2 -> do
        dotId <- emitNamedShape HouseShape (Just name) "EFocus"
        emitEdge dotId ll1
        emitEdge dotId ll2
        emitEdge dotId e1
        emitEdge dotId e2
        return dotId
      ETraitInstance _ t tr -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "ETraitInstance"
        emitEdge id2 tr
        return id1
      EFFICall _ t ll es e -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EFFICall"
        emitEdge id2 ll
        emitEdges id2 es
        emitEdge id2 e
        return id1
      EDoBlock _ is -> do
        dotId <- emitShape HouseShape "EDoBlock"
        forM_ is $
          \(p, e) -> do
            id1 <- toDot p
            emitEdge dotId id1
            emitEdge id1 e
        return dotId

instance (Dot t, Dot (Type Parameter k), Show k, CoalPretty k) => Dot (Pattern a k t) where
  toDot =
    \case
      PAnnotation _ t p -> do
        (id1, id2) <- withTypeInfo t $ emitShape ParallelogramShape "PAnnotation"
        emitEdge id2 p
        return id1
      PAny _ t -> do
        (id1, _) <- withTypeInfo t $ emitShape ParallelogramShape "PAny"
        return id1
      PVariable _ (Label t name) -> do
        (id1, _) <- withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "PVariable"
        return id1
      PConstructor _ (Label t name) ps -> do
        (id1, id2) <- withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "PConstructor"
        emitEdges id2 ps
        return id1
      PInteger _ t n -> do
        (id1, _) <- withTypeInfo t $ emitNamedShape ParallelogramShape (Just $ showt n) "PInteger"
        return id1
      PLiteral _ p -> do
        dotId <- emitShape ParallelogramShape "PLiteral"
        emitEdge dotId p
        return dotId
      PRecord _ t d mp -> do
        (id1, id2) <- withTypeInfo t $ emitShape ParallelogramShape "PRecord"
        emitEdge id2 d
        emitEdge id2 mp
        return id1
      PListCons _ t p1 p2 -> do
        (id1, id2) <- withTypeInfo t $ emitShape ParallelogramShape "PListCons"
        emitEdge id2 p1
        emitEdge id2 p2
        return id1
      PListLiteral _ t [] -> do
        (id1, id2) <- withTypeInfo t $ emitShape ParallelogramShape "PListLiteral"
        emitEdge id2 (emitShape HexagonShape "[]")
        return id1
      PListLiteral _ t ps -> do
        (id1, id2) <- withTypeInfo t $ emitShape ParallelogramShape "PListLiteral"
        emitEdges id2 ps
        return id1
      PTuple _ t ps -> do
        (id1, id2) <- withTypeInfo t $ emitShape ParallelogramShape "PTuple"
        emitEdges id2 ps
        return id1
      POr _ t p1 p2 -> do
        (id1, id2) <- withTypeInfo t $ emitShape ParallelogramShape "POr"
        emitEdges id2 p1
        emitEdges id2 p2
        return id1
      PAs _ (Label t name) p -> do
        (id1, id2) <- withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "PAs"
        emitEdge id2 p
        return id1
      PShorthand _ (Label t name) -> do
        (id1, _) <- withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "PShorthand"
        return id1
      PAtVariable _ (Label t name) -> do
        (id1, _) <- withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "PAtVariable"
        return id1
      PNamedFold _ name ll -> do
        dotId <- emitNamedShape ParallelogramShape (Just name) "PNamedFold"
        emitEdge dotId ll
        return dotId
      PTraitInstance _ t tr -> do
        (id1, id2) <- withTypeInfo t $ emitShape ParallelogramShape "PTraitInstance"
        emitEdge id2 tr
        return id1

instance Dot Operator where
  toDot =
    \case
      OLogicalNot ->
        emitShape RectangleShape "OLogicalNot"
      OLogicalOr ->
        emitShape RectangleShape "OLogicalOr"
      OLogicalAnd ->
        emitShape RectangleShape "OLogicalAnd"
      OForwardApplication ->
        emitShape RectangleShape "OForwardApplication"
      OReverseApplication ->
        emitShape RectangleShape "OReverseApplication"
      OForwardComposition ->
        emitShape RectangleShape "OForwardComposition"
      OReverseComposition ->
        emitShape RectangleShape "OReverseComposition"
      OStringConcatenation ->
        emitShape RectangleShape "OStringConcatenation"
      OListConcatenation ->
        emitShape RectangleShape "OListConcatenation"

instance (Dot t, Dot (Type Parameter k), Show k, CoalPretty k) => Dot (Clause a k t) where
  toDot =
    \case
      EClause{..} -> do
        dotId <- emitShape RectangleShape "EClause"
        emitEdge dotId clausePattern
        emitEdges dotId clauseChoices
        return dotId

instance (Dot t, Dot (Type Parameter k), Show k, CoalPretty k) => Dot (Choice Expression a k t) where
  toDot =
    \case
      CPlain _ gs e -> do
        dotId <- emitShape RectangleShape "CPlain"
        emitEdges dotId gs
        emitEdge dotId e
        return dotId

instance (Dot t, Dot (Type Parameter k), Show k, CoalPretty k) => Dot (Guard Expression a k t) where
  toDot =
    \case
      CGuard e -> do
        dotId <- emitShape RectangleShape "CGuard"
        emitEdge dotId e
        return dotId

instance (Dot t, Dot (Type Parameter k), Show k, CoalPretty k) => Dot (CompiledClause a k t) where
  toDot =
    \case
      ECompiledClause{..} -> do
        dotId <- emitShape RectangleShape "ECompiledClause"
        emitEdges dotId compiledClauseSegments
        emitEdge dotId compiledClauseExpression
        return dotId

numberedList :: [Text]
numberedList = ["#" <> showt i | i <- [1 :: Int ..]]

instance Dot Kernel.Type where
  toDot = undefined

instance Dot (Kernel.Binding Kernel.Type Int) where
  toDot =
    \case
      Kernel.Binding (Label t name) e -> do
        dotId <- emitNamedShape RectangleShape (Just name) ("Binding\\n" <> name)
        emitEdgeWithLabel "=" dotId e
        return dotId

instance Dot (Kernel.Clause Kernel.Type Int) where
  toDot =
    \case
       Kernel.Clause lls e -> do
        dotId <- emitShape RectangleShape "Clause"
        emitEdges dotId lls
        emitEdge dotId e
        return dotId

--instance Dot Kernel.Type (Kernel.Clause Kernel.Type (DotGen Kernel.Type Int)) where
--  toDot =
--    \case
--      Kernel.Clause lls e ->
--        fromNode (emitRectangle "Clause" Nothing) $ do
--          emitEdgesTo lls
--          emitEdgeTo e

instance Dot (Kernel.Focus Kernel.Type) where
  toDot =
    \case
      Kernel.Focus name ll1 ll2 -> do
        dotId <- emitShape RectangleShape ("Focus\\n" <> name)
        emitEdge dotId ll1
        emitEdge dotId ll2
        return dotId

--emitOp :: (Dot t a) => Text -> [a] -> DotGen t Int
--emitOp text = fromNode (emitRectangle text Nothing) . emitEdgesTo

instance Dot (Kernel.Op Int) where
  toDot =
    \case
      Kernel.OEqInt32 op1 op2 ->
        emitOp "OEqInt32" [op1, op2]
      Kernel.OEqInt64 op1 op2 ->
        emitOp "OEqInt64" [op1, op2]
      Kernel.OEqFloat op1 op2 ->
        emitOp "OEqFloat" [op1, op2]
      Kernel.OEqDouble op1 op2 ->
        emitOp "OEqDouble" [op1, op2]
      Kernel.OEqChar op1 op2 ->
        emitOp "OEqChar" [op1, op2]
      Kernel.OEqBool op1 op2 ->
        emitOp "OEqBool" [op1, op2]
      Kernel.ONeInt32 op1 op2 ->
        emitOp "ONeInt32" [op1, op2]
      Kernel.ONeInt64 op1 op2 ->
        emitOp "ONeInt64" [op1, op2]
      Kernel.ONeFloat op1 op2 ->
        emitOp "ONeFloat" [op1, op2]
      Kernel.ONeDouble op1 op2 ->
        emitOp "ONeDouble" [op1, op2]
      Kernel.ONeChar op1 op2 ->
        emitOp "ONeChar" [op1, op2]
      Kernel.ONeBool op1 op2 ->
        emitOp "ONeBool" [op1, op2]
      Kernel.OLtInt32 op1 op2 ->
        emitOp "OLtInt32" [op1, op2]
      Kernel.OLtInt64 op1 op2 ->
        emitOp "OLtInt64" [op1, op2]
      Kernel.OLtFloat op1 op2 ->
        emitOp "OLtFloat" [op1, op2]
      Kernel.OLtDouble op1 op2 ->
        emitOp "OLtDouble" [op1, op2]
      Kernel.OGtInt32 op1 op2 ->
        emitOp "OGtInt32" [op1, op2]
      Kernel.OGtInt64 op1 op2 ->
        emitOp "OGtInt64" [op1, op2]
      Kernel.OGtFloat op1 op2 ->
        emitOp "OGtFloat" [op1, op2]
      Kernel.OGtDouble op1 op2 ->
        emitOp "OGtDouble" [op1, op2]
      Kernel.OLteInt32 op1 op2 ->
        emitOp "OLteInt32" [op1, op2]
      Kernel.OLteInt64 op1 op2 ->
        emitOp "OLteInt64" [op1, op2]
      Kernel.OLteFloat op1 op2 ->
        emitOp "OLteFloat" [op1, op2]
      Kernel.OLteDouble op1 op2 ->
        emitOp "OLteDouble" [op1, op2]
      Kernel.OGteInt32 op1 op2 ->
        emitOp "OGteInt32" [op1, op2]
      Kernel.OGteInt64 op1 op2 ->
        emitOp "OGteInt64" [op1, op2]
      Kernel.OGteFloat op1 op2 ->
        emitOp "OGteFloat" [op1, op2]
      Kernel.OGteDouble op1 op2 ->
        emitOp "OGteDouble" [op1, op2]
      Kernel.OAddInt32 op1 op2 ->
        emitOp "OAddInt32" [op1, op2]
      Kernel.OAddInt64 op1 op2 ->
        emitOp "OAddInt64" [op1, op2]
      Kernel.OAddFloat op1 op2 ->
        emitOp "OAddFloat" [op1, op2]
      Kernel.OAddDouble op1 op2 ->
        emitOp "OAddDouble" [op1, op2]
      Kernel.OSubInt32 op1 op2 ->
        emitOp "OSubInt32" [op1, op2]
      Kernel.OSubInt64 op1 op2 ->
        emitOp "OSubInt64" [op1, op2]
      Kernel.OSubFloat op1 op2 ->
        emitOp "OSubFloat" [op1, op2]
      Kernel.OSubDouble op1 op2 ->
        emitOp "OSubDouble" [op1, op2]
      Kernel.OMulInt32 op1 op2 ->
        emitOp "OMulInt32" [op1, op2]
      Kernel.OMulInt64 op1 op2 ->
        emitOp "OMulInt64" [op1, op2]
      Kernel.OMulFloat op1 op2 ->
        emitOp "OMulFloat" [op1, op2]
      Kernel.OMulDouble op1 op2 ->
        emitOp "OMulDouble" [op1, op2]
      Kernel.ODivInt32 op1 op2 ->
        emitOp "ODivInt32" [op1, op2]
      Kernel.ODivInt64 op1 op2 ->
        emitOp "ODivInt64" [op1, op2]
      Kernel.ODivFloat op1 op2 ->
        emitOp "ODivFloat" [op1, op2]
      Kernel.ODivDouble op1 op2 ->
        emitOp "ODivDouble" [op1, op2]
      Kernel.OOr op1 op2 ->
        emitOp "OOr" [op1, op2]
      Kernel.OAnd op1 op2 ->
        emitOp "OAnd" [op1, op2]
      Kernel.ONot op1 ->
        emitOp "ONot" [op1]
      Kernel.ONegFloat op1 ->
        emitOp "ONegFloat" [op1]
      Kernel.ONegDouble op1 ->
        emitOp "ONegDouble" [op1]

emitOp :: (Dot t) => Text -> [a] -> DotGen t
emitOp text = undefined -- fromNode (emitRectangle text Nothing) . emitEdgesTo

instance Dot (Kernel.Expr Kernel.Type) where
  toDot =
    cata $
      \case
        Kernel.EVar (Label t name) ->
          undefined -- emitRectangle ("EVar\\n" <> name) (Just t)
        Kernel.ELet bs e ->
          undefined
          --fromNode (emitRectangle "ELet" Nothing) $ do
          --  emitEdgesTo bs
          --  emitEdgeWithLabelTo "in" e
--        Kernel.ELit p ->
--          emitRectangle ("ELit\\n" <> Text.pack (show p)) Nothing
--        Kernel.ELam lls e -> do
--          fromNode (emitHouse "ELam" Nothing) $ do
--            emitEdgesTo lls
--            emitEdgeTo e
--        Kernel.EApp t e es ->
--          fromNode (emitDiamond "EApp" (Just t)) $ do
--            emitEdgeTo e
--            emitEdgesTo es
--        Kernel.EIf e1 e2 e3 ->
--          fromNode (emitRectangle "EIf" Nothing) $ do
--            emitEdgeTo e1
--            emitEdgeWithLabelTo "then" e2
--            emitEdgeWithLabelTo "else" e3
--        Kernel.EOp op ->
--          fromNode (emitRectangle "EOp\\n" Nothing) $ do
--            emitEdgeTo op
--        Kernel.EMat t e cs ->
--          fromNode (emitRectangle "EMat" (Just t)) $ do
--            emitEdgeTo e
--            emitEdgesTo cs
--        Kernel.EExt fname e1 e2 ->
--          fromNode (emitHexagon ("EExt\\n" <> fname) Nothing) $ do
--            emitEdgeTo e1
--            emitEdgeTo e2
--        Kernel.ENil ->
--          emitHexagon "ENil" Nothing
--        Kernel.ESel f e1 e2 ->
--          fromNode (emitHexagon "ESel" Nothing) $ do
--            emitEdgeTo f
--            emitEdgeTo e1
--            emitEdgeTo e2
--        Kernel.ECall (Label t name) es e ->
--          fromNode (emitHexagon ("ECall\\n" <> name) (Just t)) $ do
--            emitEdgesTo es
--            emitEdgeTo e
--        Kernel.EMem e ->
--          fromNode (emitHexagon "EMem" Nothing) $
--            emitEdgeTo e

--instance Dot Kernel.Type (Kernel.Expr Kernel.Type) where
--  toDot =
--    cata $
--      \case
--        Kernel.EVar (Label t name) ->
--          emitRectangle ("EVar\\n" <> name) (Just t)
--        Kernel.ELet bs e ->
--          fromNode (emitRectangle "ELet" Nothing) $ do
--            emitEdgesTo bs
--            emitEdgeWithLabelTo "in" e
--        Kernel.ELit p ->
--          emitRectangle ("ELit\\n" <> Text.pack (show p)) Nothing
--        Kernel.ELam lls e -> do
--          fromNode (emitHouse "ELam" Nothing) $ do
--            emitEdgesTo lls
--            emitEdgeTo e
--        Kernel.EApp t e es ->
--          fromNode (emitDiamond "EApp" (Just t)) $ do
--            emitEdgeTo e
--            emitEdgesTo es
--        Kernel.EIf e1 e2 e3 ->
--          fromNode (emitRectangle "EIf" Nothing) $ do
--            emitEdgeTo e1
--            emitEdgeWithLabelTo "then" e2
--            emitEdgeWithLabelTo "else" e3
--        Kernel.EOp op ->
--          fromNode (emitRectangle "EOp\\n" Nothing) $ do
--            emitEdgeTo op
--        Kernel.EMat t e cs ->
--          fromNode (emitRectangle "EMat" (Just t)) $ do
--            emitEdgeTo e
--            emitEdgesTo cs
--        Kernel.EExt fname e1 e2 ->
--          fromNode (emitHexagon ("EExt\\n" <> fname) Nothing) $ do
--            emitEdgeTo e1
--            emitEdgeTo e2
--        Kernel.ENil ->
--          emitHexagon "ENil" Nothing
--        Kernel.ESel f e1 e2 ->
--          fromNode (emitHexagon "ESel" Nothing) $ do
--            emitEdgeTo f
--            emitEdgeTo e1
--            emitEdgeTo e2
--        Kernel.ECall (Label t name) es e ->
--          fromNode (emitHexagon ("ECall\\n" <> name) (Just t)) $ do
--            emitEdgesTo es
--            emitEdgeTo e
--        Kernel.EMem e ->
--          fromNode (emitHexagon "EMem" Nothing) $
--            emitEdgeTo e

instance Dot (Kernel.Object Kernel.Type (Kernel.Expr Kernel.Type)) where
  toDot =
    \case
      Kernel.OFunction name lls e ->
        undefined
--        fromNode (emitParallelogram ("OFunction\\n" <> name) Nothing) $ do
--          emitEdgesTo lls
--          emitEdgeTo e
      Kernel.OConstant name e ->
        undefined
--        fromNode (emitParallelogram ("OConstant\\n" <> name) Nothing) $ do
--          emitEdgeTo e
      Kernel.OExternal{} ->
        undefined
--        emitParallelogram "TODO" Nothing
      Kernel.OData{} ->
        undefined
--        emitParallelogram "TODO" Nothing

--instance Dot Kernel.Type (Kernel.Object Kernel.Type (Kernel.Expr Kernel.Type)) where
--  toDot =
--    \case
--      Kernel.OFunction name lls e ->
--        fromNode (emitParallelogram ("OFunction\\n" <> name) Nothing) $ do
--          emitEdgesTo lls
--          emitEdgeTo e
--      Kernel.OConstant name e ->
--        fromNode (emitParallelogram ("OConstant\\n" <> name) Nothing) $ do
--          emitEdgeTo e
--      Kernel.OExternal{} ->
--        emitParallelogram "TODO" Nothing
--      Kernel.OData{} ->
--        emitParallelogram "TODO" Nothing

instance Dot (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)) where
  toDot =
    \case
      Kernel.Module{..} -> do
        undefined

--instance Dot Kernel.Type (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)) where
--  toDot =
--    \case
--      Kernel.Module{..} -> do
--        nid <- emitEllipse moduleName Nothing
--        traverse_ toDot moduleObjects
--        return nid

shapeToDotSyntax :: DotShape -> Text
shapeToDotSyntax =
  \case
    RectangleShape ->
      "rectangle"
    EllipseShape ->
      "ellipse"
    ParallelogramShape ->
      "parallelogram"
    HexagonShape ->
      "hexagon"
    FolderShape ->
      "folder"
    TriangleShape ->
      "triangle"
    DiamondShape ->
      "diamond"
    HouseShape ->
      "house"
    NoteShape ->
      "note"

edgeLabelToDotSyntax :: Maybe Text -> Text
edgeLabelToDotSyntax =
  \case
    Nothing ->
      ""
    Just ll ->
      " [label=\"  " <> ll <> "\", labeldistance=2]"

labelToDotSyntax :: Text -> Maybe Text -> Text
labelToDotSyntax label Nothing = "\"" <> escapeQuotes label <> "\""
labelToDotSyntax label (Just name) = "<" <> escapeHtmlEntities (escapeHtmlEntities label) <> "<BR/>" <> inBold (escapeHtmlEntities name) <> "<BR/>" <> ">"

escapeHtmlEntities :: Text -> Text
escapeHtmlEntities = Text.replace "<" "&lt;" . Text.replace ">" "&gt;"

{-# INLINE escapeQuotes #-}
escapeQuotes :: Text -> Text
escapeQuotes = Text.replace "\"" "\\\""

{-# INLINE inBold #-}
inBold :: Text -> Text
inBold text = "<B>" <> text <> "</B>"

generateDotSyntax :: (Dot a) => a -> Text
generateDotSyntax ast =
  Text.unlines $
    [ "digraph AST {"
    , "  node [shape=box];"
    , "  edge [arrowhead=none];"
    ]
      <> map ("  " <>) (reverse dotNodes ++ dotEdges)
      <> ["}"]
 where
  initialState = DotState 0 [] []
  (_, finalState) = runState (toDot ast) initialState
  dotNodes =
    [ showt dotNodeId
      <> " [shape="
      <> shapeToDotSyntax dotNodeShape
      <> ", label="
      <> labelToDotSyntax dotNodeLabel dotNodeName
      <> "];"
    | DotNode{..} <- dotStateNodes finalState
    ]
  dotEdges =
    [ showt dotEdgeFrom
      <> " -> "
      <> showt dotEdgeTo
      <> edgeLabelToDotSyntax dotEdgeLabel
      <> ";"
    | DotEdge{..} <- dotStateEdges finalState
    ]

prettyType :: (CoalPretty t) => t -> Text
prettyType p = Text.replace "->" "→" (renderStrict $ layoutPretty defaultLayoutOptions $ prettyCoal p)
