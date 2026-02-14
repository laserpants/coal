{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Graphviz.ProtoDot where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Type.Scheme
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
    \case
      NameExport a name ->
        emitShape RectangleShape "NameExport"
      TypeExport a name names ->
        emitShape RectangleShape "TypeExport"

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
    \case
      ProtoModule{..} -> do
        id1 <- emitShape RectangleShape "Module"
        id2 <- toDot protoOmoduleExportList
        emitEdge id1 id2
        ids <- forM protoOmoduleDefinitions toDot
        forM_ ids $ emitEdge id1
        pure id1

instance ProtoDot (ProtoDefinition a k t) where
  toDot =
    \case
      ProtoDType a name def ->
        emitShape EllipseShape "DType"
      ProtoDTypeAlias a name def ->
        emitShape EllipseShape "DTypeAlias"
      ProtoDFunction a name def ->
        emitShape EllipseShape "DFunction"
      ProtoDFunctionGroup a name defs ->
        emitShape EllipseShape "DFunctionGroup"
      ProtoDFold a name def ->
        emitShape EllipseShape "DFold"
      ProtoDLet a name def ->
        emitShape EllipseShape "DLet"
      ProtoDImport a path imports ->
        emitShape EllipseShape "DImport"
      ProtoDQualifiedImport a path ->
        emitShape EllipseShape "DQualifiedImport"
      ProtoDTrait a name def ->
        emitShape EllipseShape "DTrait"
      ProtoDInstance a def ->
        emitShape EllipseShape "DInstance"

instance ProtoDot (ProtoTypeDefinition a k t) where
  toDot =
    \case
      ProtoTypeDefinition{..} -> do
        id1 <- emitShape EllipseShape "TypeDefinition"
        forM_ protoOtypeDefinitionParameters $
          \p -> do
            idn <- toDot p
            emitEdge id1 idn
        forM_ protoOtypeDefinitionConstructors $
          \ctor -> do
            idn <- toDot ctor
            emitEdge id1 idn
        pure id1

instance ProtoDot (ProtoFunctionDefinition a k t) where
  toDot =
    \case
      ProtoFunctionDefinition{..} -> do
        id1 <- emitShape EllipseShape "FunctionDefinition"
        undefined

instance ProtoDot (ProtoLetDefinition a k t) where
  toDot =
    \case
      ProtoLetDefinition{..} -> do
        id1 <- emitShape EllipseShape "LetDefinition"
        undefined

instance ProtoDot (ProtoFoldDefinition a k t) where
  toDot =
    \case
      ProtoFoldDefinition{..} -> do
        id1 <- emitShape EllipseShape "FoldDefinition"
        undefined

instance ProtoDot (ProtoTraitDefinition a k) where
  toDot =
    \case
      ProtoTraitDefinition{..} -> do
        id1 <- emitShape EllipseShape "TraitDefinition"
        undefined

instance ProtoDot (ProtoInstanceDefinition a k t) where
  toDot =
    \case
      ProtoInstanceDefinition{..} -> do
        id1 <- emitShape EllipseShape "InstanceDefinition"
        undefined

instance ProtoDot (ProtoAliasDefinition a k) where
  toDot =
    \case
      ProtoAliasDefinition{..} -> do
        id1 <- emitShape EllipseShape "AliasDefinition"
        undefined

instance ProtoDot (DataConstructor o k t) where
  toDot =
    \case
      DataConstructor{..} -> do
        id1 <- emitShape EllipseShape "DataConstructor"
        id2 <- toDot constructorScheme
        emitEdge id1 id2
        pure id1

instance ProtoDot (Parameter k) where
  toDot =
    \case
      Parameter{..} ->
        undefined

instance ProtoDot (Scheme o k t) where
  toDot =
    \case
      Forall{..} ->
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
        emitShape RectangleShape "EAnnotation"
      EApplication a t e es ->
        emitShape RectangleShape "EApplication"
      ELambda a ps e ->
        emitShape RectangleShape "ELambda"
      ELet a bs e ->
        emitShape RectangleShape "ELet"
      ERecursiveLet a p e1 e2 ->
        emitShape RectangleShape "ERecursiveLet"
      EVariable a ll ->
        emitShape RectangleShape "EVariable"
      EConstructor a ll ->
        emitShape RectangleShape "EConstructor"
      ELiteral a p ->
        emitShape RectangleShape "ELiteral"
      EIf a t e1 e2 e3 ->
        emitShape RectangleShape "EIf"
      EOperator a t op ->
        emitShape RectangleShape "EOperator"
      ERecord a t d me ->
        emitShape RectangleShape "ERecord"
      EListCons a t e1 e2 ->
        emitShape RectangleShape "EListCons"
      EListLiteral a t es ->
        emitShape RectangleShape "EListLiteral"
      ETuple a t es ->
        emitShape RectangleShape "ETuple"
      EMatch a t e cs ->
        emitShape RectangleShape "EMatch"
      ELambdaMatch a t cs ->
        emitShape RectangleShape "ELambdaMatch"
      ECompiledMatch a t e cs ->
        emitShape RectangleShape "ECompiledMatch"
      EFold a t es cs ->
        emitShape RectangleShape "EFold"
      ESelect a ll e ->
        emitShape RectangleShape "ESelect"
      EFocus a name ll1 ll2 e1 e2 ->
        emitShape RectangleShape "EFocus"
      ETraitInstance a t tr ->
        emitShape RectangleShape "ETraitInstance"
      EFFICall a t ll es e ->
        emitShape RectangleShape "EFFICall"
      EDoBlock a is ->
        emitShape RectangleShape "EDoBlock"

instance ProtoDot (Pattern a k t) where
  toDot =
    \case
      PAnnotation a t p ->
        emitShape ParallelogramShape "PAnnotation"
      PAny a t ->
        emitShape ParallelogramShape "PAny"
      PVariable a ll ->
        emitShape ParallelogramShape "PVariable"
      PConstructor a ll ps ->
        emitShape ParallelogramShape "PConstructor"
      PInteger a t n ->
        emitShape ParallelogramShape "PInteger"
      PLiteral a p ->
        emitShape ParallelogramShape "PLiteral"
      PRecord a t d mp ->
        emitShape ParallelogramShape "PRecord"
      PListCons a t p1 p2 ->
        emitShape ParallelogramShape "PListCons"
      PListLiteral a t ps ->
        emitShape ParallelogramShape "PListLiteral"
      PTuple a t ps ->
        emitShape ParallelogramShape "PTuple"
      POr a t p1 p2 ->
        emitShape ParallelogramShape "POr"
      PAs a ll p ->
        emitShape ParallelogramShape "PAs"
      PShorthand a ll ->
        emitShape ParallelogramShape "PShorthand"
      PAtVariable a ll ->
        emitShape ParallelogramShape "PAtVariable"
      PNamedFold a name ll ->
        emitShape ParallelogramShape "PNamedFold"
      PTraitInstance a t tr ->
        emitShape ParallelogramShape "PTraitInstance"

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
