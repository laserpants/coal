{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Graphviz.ProtoDot where

import Coal.Language.Module.Export (Export (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Control.Monad.State
import Data.Text (Text)

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
  , dotNodeX :: Text
  , dotNodeShape :: DotShape
  }
  deriving (Show, Eq, Read)

data DotEdge = DotEdge
  { dotEdgeFrom :: Int
  , dotEdgeTo :: Int
  , dotEdgeLabel :: Maybe Text
  }
  deriving (Show, Eq, Read)

data ProtoDotState = ProtoDotState
  { dotStateSupply :: Int
  , dotStateNodes :: [DotNode]
  , dotStateEdges :: [DotEdge]
  }
  deriving (Show, Eq, Read)

instance Supply ProtoDotState where
  updateSupply f ProtoDotState{..} = ProtoDotState{dotStateSupply = f dotStateSupply, ..}
  getSupply = dotStateSupply

insertNode :: DotNode -> ProtoDotState -> ProtoDotState
insertNode node ProtoDotState{..} =
  ProtoDotState
    { dotStateNodes = node : dotStateNodes
    , ..
    }

insertEdge :: Int -> Int -> Maybe Text -> ProtoDotState -> ProtoDotState
insertEdge from to label ProtoDotState{..} =
  ProtoDotState
    { dotStateEdges = DotEdge from to label : dotStateEdges
    , ..
    }

type ProtoDotGen = State ProtoDotState

freshId :: ProtoDotGen Int
freshId = supplied id

emitNode :: DotNode -> ProtoDotGen ()
emitNode = modify . insertNode

emitEdge :: Int -> Int -> ProtoDotGen ()
emitEdge from to = modify (insertEdge from to Nothing)

emitEdgeWithLabel :: Int -> Int -> Text -> ProtoDotGen ()
emitEdgeWithLabel from to label = modify (insertEdge from to (Just label))

emitShape :: DotShape -> Text -> ProtoDotGen Int
emitShape shape t = do
  nid <- freshId
  emitNode (DotNode nid t shape)
  return nid

class ProtoDot a where
  toDot :: a -> ProtoDotGen Int

instance (ProtoDot a) => ProtoDot (Maybe a) where
  toDot =
    \case
      Nothing -> do
        emitShape RectangleShape "Nothing"
      Just d -> do
        id1 <- emitShape RectangleShape "Just"
        id2 <- toDot d
        emitEdge id1 id2
        return id1

instance ProtoDot (Label t) where
  toDot =
    \case
      Label t name ->
        emitShape NoteShape "Label"

instance ProtoDot (Export a) where
  toDot =
    undefined

instance ProtoDot (ModuleExportList a) where
  toDot =
    \case
      Exports exports -> do
        id1 <- emitShape FolderShape "Exports"
        forM_ exports $
          \export -> do
            idn <- toDot export
            emitEdge id1 idn
        return id1
      ExportAll ->
        emitShape FolderShape "ExportsAll"

instance ProtoDot (ProtoModule a k t) where
  toDot =
    undefined

instance ProtoDot (ProtoDefinition a k t) where
  toDot =
    \case
      ProtoDType a name def ->
        undefined
      ProtoDTypeAlias a name def ->
        undefined
      ProtoDFunction a name def ->
        undefined
      ProtoDFunctionGroup a name defs ->
        undefined
      ProtoDFold a name def ->
        undefined
      ProtoDLet a name def ->
        undefined
      ProtoDImport a path imports ->
        undefined
      ProtoDQualifiedImport a path ->
        undefined
      ProtoDTrait a name def ->
        undefined
      ProtoDInstance a def ->
        undefined

instance ProtoDot (ProtoTypeDefinition a k t) where
  toDot =
    \case
      ProtoTypeDefinition{..} ->
        undefined

instance ProtoDot (ProtoFunctionDefinition a k t) where
  toDot =
    \case
      ProtoFunctionDefinition{..} ->
        undefined

instance ProtoDot (ProtoLetDefinition a k t) where
  toDot =
    \case
      ProtoLetDefinition{..} ->
        undefined

instance ProtoDot (ProtoFoldDefinition a k t) where
  toDot =
    \case
      ProtoFoldDefinition{..} ->
        undefined

instance ProtoDot (ProtoTraitDefinition a k) where
  toDot =
    \case
      ProtoTraitDefinition{..} ->
        undefined

instance ProtoDot (ProtoInstanceDefinition a k t) where
  toDot =
    \case
      ProtoInstanceDefinition{..} ->
        undefined

instance ProtoDot (ProtoAliasDefinition a k) where
  toDot =
    \case
      ProtoAliasDefinition{..} ->
        undefined

instance ProtoDot (Binding Expression a k t) where
  toDot =
    \case
      BPattern a p e ->
        undefined
      BFunction a name ps e ->
        undefined

instance ProtoDot (Expression a k t) where
  toDot =
    \case
      EAnnotation a t e ->
        undefined
      EApplication a t e es ->
        undefined
      ELambda a ps e ->
        undefined
      ELet a bs e ->
        undefined
      ERecursiveLet a p e1 e2 ->
        undefined
      EVariable a ll ->
        undefined
      EConstructor a ll ->
        undefined
      ELiteral a p ->
        undefined
      EIf a t e1 e2 e3 ->
        undefined
      EOperator a t op ->
        undefined
      ERecord a t d me ->
        undefined
      EListCons a t e1 e2 ->
        undefined
      EListLiteral a t es ->
        undefined
      ETuple a t es ->
        undefined
      EMatch a t e cs ->
        undefined
      ELambdaMatch a t cs ->
        undefined
      ECompiledMatch a t e cs ->
        undefined
      EFold a t es cs ->
        undefined
      ESelect a ll e ->
        undefined
      EFocus a name ll1 ll2 e1 e2 ->
        undefined
      ETraitInstance a t tr ->
        undefined
      EFFICall a t ll es e ->
        undefined
      EDoBlock a is ->
        undefined

instance ProtoDot (Pattern a k t) where
  toDot =
    \case
      PAnnotation a t p ->
        undefined
      PAny a t ->
        undefined
      PVariable a ll ->
        undefined
      PConstructor a ll ps ->
        undefined
      PInteger a t n ->
        undefined
      PLiteral a p ->
        undefined
      PRecord a t d mp ->
        undefined
      PListCons a t p1 p2 ->
        undefined
      PListLiteral a t ps ->
        undefined
      PTuple a t ps ->
        undefined
      POr a t p1 p2 ->
        undefined
      PAs a ll p ->
        undefined
      PShorthand a ll ->
        undefined
      PAtVariable a ll ->
        undefined
      PNamedFold a name ll ->
        undefined
      PTraitInstance a t tr ->
        undefined

instance ProtoDot (Clause a k t) where
  toDot =
    \case
      EClause a p cs ->
        undefined

instance ProtoDot (Choice Expression a k t) where
  toDot =
    \case
      CPlain a gs e ->
        undefined

instance ProtoDot (Guard Expression a k t) where
  toDot =
    \case
      CGuard e ->
        undefined

instance ProtoDot (CompiledClause a k t) where
  toDot =
    \case
      ECompiledClause a lls e ->
        undefined

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
