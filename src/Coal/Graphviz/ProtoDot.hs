{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Graphviz.ProtoDot (generateDotSyntax) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Path (principalPath)
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as Text
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
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

emitNamedShape :: DotShape -> Maybe Text -> Text -> ProtoDotGen Int
emitNamedShape shape name label = do
  nid <- freshId
  emitNode (DotNode nid label name shape)
  return nid

emitShape :: DotShape -> Text -> ProtoDotGen Int
emitShape shape = emitNamedShape shape Nothing

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
        emitNamedShape NoteShape (Just name) "Label"

instance ProtoDot (Export a) where
  toDot =
    \case
      NameExport a name ->
        emitNamedShape RectangleShape (Just name) "NameExport"
      TypeExport a name names ->
        emitNamedShape RectangleShape (Just name) "TypeExport"

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

instance (ProtoDot t) => ProtoDot (ProtoModule a k t) where
  toDot =
    \case
      ProtoModule{..} -> do
        id1 <- emitNamedShape RectangleShape (Just $ principalPath protoOmodulePath) "Module"
        id2 <- toDot protoOmoduleExportList
        emitEdge id1 id2
        ids <- forM protoOmoduleDefinitions toDot
        forM_ ids $ emitEdge id1
        return id1

instance (ProtoDot t) => ProtoDot (ProtoDefinition a k t) where
  toDot =
    \case
      ProtoDType a name def -> do
        id1 <- emitNamedShape TriangleShape (Just name) "DType"
        id2 <- toDot def
        emitEdge id1 id2
        return id1
      ProtoDTypeAlias a name def ->
        emitNamedShape TriangleShape (Just name) "DTypeAlias"
      ProtoDFunction a name def -> do
        id1 <- emitNamedShape TriangleShape (Just name) "DFunction"
        id2 <- toDot def
        emitEdge id1 id2
        return id1
      ProtoDLet a name def -> do
        id1 <- emitNamedShape TriangleShape (Just name) "DLet"
        id2 <- toDot def
        emitEdge id1 id2
        return id1
      ProtoDFunctionGroup a name defs ->
        emitNamedShape TriangleShape (Just name) "DFunctionGroup"
      ProtoDFold a name def ->
        emitNamedShape TriangleShape (Just name) "DFold"
      ProtoDImport a path imports ->
        emitShape TriangleShape "DImport"
      ProtoDQualifiedImport a path ->
        emitShape TriangleShape "DQualifiedImport"
      ProtoDTrait a name def ->
        emitNamedShape TriangleShape (Just name) "DTrait"
      ProtoDInstance a def ->
        emitShape TriangleShape "DInstance"

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
        return id1

instance (ProtoDot t) => ProtoDot (ProtoFunctionDefinition a k t) where
  toDot =
    \case
      ProtoFunctionDefinition{..} -> do
        id1 <- emitShape EllipseShape "FunctionDefinition"
        -- id2 <- toDot protoOfunctionDefinitionAnnotation
        -- id3 <- toDot protoOfunctionDefinitionType
        -- id3 <- toDot protoOfunctionDefinitionPatterns
        id4 <- toDot protoOfunctionDefinitionExpression
        emitEdge id1 id4
        return id1

instance ProtoDot (ProtoLetDefinition a k t) where
  toDot =
    \case
      ProtoLetDefinition{..} -> do
        id1 <- emitShape EllipseShape "LetDefinition"
        -- TODO
        return id1

instance ProtoDot (ProtoFoldDefinition a k t) where
  toDot =
    \case
      ProtoFoldDefinition{..} -> do
        id1 <- emitShape EllipseShape "FoldDefinition"
        -- TODO
        return id1

instance ProtoDot (ProtoTraitDefinition a k) where
  toDot =
    \case
      ProtoTraitDefinition{..} -> do
        id1 <- emitShape EllipseShape "TraitDefinition"
        -- TODO
        return id1

instance ProtoDot (ProtoInstanceDefinition a k t) where
  toDot =
    \case
      ProtoInstanceDefinition{..} -> do
        id1 <- emitShape EllipseShape "InstanceDefinition"
        -- TODO
        return id1

instance ProtoDot (ProtoAliasDefinition a k) where
  toDot =
    \case
      ProtoAliasDefinition{..} -> do
        id1 <- emitShape EllipseShape "AliasDefinition"
        -- TODO
        return id1

instance ProtoDot (DataConstructor o k t) where
  toDot =
    \case
      DataConstructor{..} -> do
        id1 <- emitNamedShape EllipseShape (Just constructorName) "DataConstructor"
        id2 <- toDot constructorScheme
        emitEdge id1 id2
        return id1

instance ProtoDot (Parameter k) where
  toDot =
    \case
      Parameter{..} ->
        emitNamedShape RectangleShape (Just parameterName) "Parameter"

instance ProtoDot (Scheme o k t) where
  toDot =
    \case
      Forall{..} ->
        emitShape RectangleShape "Scheme"

instance ProtoDot (Type TypeIndex Kind) where
  toDot t = do
    emitShape HexagonShape (prettyType t)

instance ProtoDot (Binding Expression a k t) where
  toDot =
    \case
      BPattern a p e ->
        emitShape RectangleShape "BPattern"
      BFunction a name ps e ->
        emitNamedShape RectangleShape (Just name) "BFunction"

withTypeInfo :: (ProtoDot t) => t -> ProtoDotGen Int -> ProtoDotGen Int
withTypeInfo t e = do
  id1 <- toDot t
  id2 <- e
  emitEdge id1 id2
  return id1

instance (ProtoDot t) => ProtoDot (Expression a k t) where
  toDot =
    \case
      EAnnotation a t e ->
        emitShape HouseShape "EAnnotation"
      EApplication a t e es -> do
        id1 <- withTypeInfo t $ emitShape HouseShape "EApplication"
        id2 <- toDot e
        emitEdge id1 id2
        ids <- forM es toDot
        forM_ ids $ emitEdge id1
        return id1
      ELambda a ps e ->
        emitShape HouseShape "ELambda"
      ELet a bs e ->
        emitShape HouseShape "ELet"
      ERecursiveLet a p e1 e2 ->
        emitShape HouseShape "ERecursiveLet"
      EVariable a (Label t name) -> do
        withTypeInfo t $ emitNamedShape HouseShape (Just name) "EVariable"
      EConstructor a ll ->
        emitShape HouseShape "EConstructor"
      ELiteral a p ->
        emitShape HouseShape "ELiteral"
      EIf a t e1 e2 e3 ->
        emitShape HouseShape "EIf"
      EOperator a t op ->
        emitShape HouseShape "EOperator"
      ERecord a t d me ->
        emitShape HouseShape "ERecord"
      EListCons a t e1 e2 ->
        emitShape HouseShape "EListCons"
      EListLiteral a t es ->
        emitShape HouseShape "EListLiteral"
      ETuple a t es ->
        emitShape HouseShape "ETuple"
      EMatch a t e cs ->
        emitShape HouseShape "EMatch"
      ELambdaMatch a t cs ->
        emitShape HouseShape "ELambdaMatch"
      ECompiledMatch a t e cs ->
        emitShape HouseShape "ECompiledMatch"
      EFold a t es cs ->
        emitShape HouseShape "EFold"
      ESelect a ll e ->
        emitShape HouseShape "ESelect"
      EFocus a name ll1 ll2 e1 e2 ->
        emitShape HouseShape "EFocus"
      ETraitInstance a t tr ->
        emitShape HouseShape "ETraitInstance"
      EFFICall a t ll es e ->
        emitShape HouseShape "EFFICall"
      EDoBlock a is ->
        emitShape HouseShape "EDoBlock"

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
      EClause{..} -> do
        id1 <- emitShape RectangleShape "EClause"
        id2 <- toDot clausePattern
        emitEdge id1 id2
        ids <- forM clauseChoices toDot
        forM_ ids $ emitEdge id1
        return id1

instance ProtoDot (Choice Expression a k t) where
  toDot =
    \case
      CPlain a gs e ->
        emitShape RectangleShape "CPlain"

instance ProtoDot (Guard Expression a k t) where
  toDot =
    \case
      CGuard e ->
        emitShape RectangleShape "CGuard"

instance ProtoDot (CompiledClause a k t) where
  toDot =
    \case
      ECompiledClause{..} ->
        emitShape RectangleShape "ECompiledClause"

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

labelToDotSyntax label Nothing = "\"" <> escapeQuotes label <> "\""
labelToDotSyntax label (Just name) = "<" <> escapeQuotes label <> "<BR/>" <> (inBold name <> "<BR/>") <> ">"

{-# INLINE escapeQuotes #-}
escapeQuotes :: Text -> Text
escapeQuotes = Text.replace "\"" "\\\""

inBold :: Text -> Text
inBold text = "<B>" <> text <> "</B>"

generateDotSyntax :: (ProtoDot a) => a -> Text
generateDotSyntax ast =
  Text.unlines $
    [ "digraph AST {"
    , "  node [shape=box];"
    , "  edge [arrowhead=none];"
    ]
      <> map ("  " <>) (reverse dotNodes ++ dotEdges)
      <> ["}"]
 where
  initialState = ProtoDotState 0 [] []
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

-- TODO
prettyType :: (Pretty t) => t -> Text
prettyType p = renderStrict . layoutPretty defaultLayoutOptions $ pretty p
