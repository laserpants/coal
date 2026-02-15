{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Graphviz.ProtoDot (generateDotSyntax) where

import Coal.Language.Module.Import (Import (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language
import Coal.Language.Module.Export (Export (..))
import Coal.Language.Module.Path (principalPath)
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Control.Monad.State
import Data.Text (Text)
import Data.Foldable (foldrM)
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

forEdges :: (Foldable f) => Int -> f Int -> ProtoDotGen ()
forEdges from ids = forM_ ids $ emitEdge from

emitEdges :: (Traversable f, ProtoDot a) => Int -> f a -> ProtoDotGen ()
emitEdges from tos = do
  ids <- forM tos toDot
  forEdges from ids

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

instance ProtoDot t => ProtoDot (Label t) where
  toDot =
    \case
      Label t name ->
        withTypeInfo t $ emitNamedShape NoteShape (Just name) "Label"

instance ProtoDot (Import a) where
  toDot =
    \case
      NameImport _ name -> 
        emitNamedShape RectangleShape (Just name) "NameImport"
      TypeImport _ name names -> do
        emitNamedShape RectangleShape (Just name) "TypeImport"

instance ProtoDot (Export a) where
  toDot =
    \case
      NameExport _ name ->
        emitNamedShape RectangleShape (Just name) "NameExport"
      TypeExport _ name names ->
        emitNamedShape RectangleShape (Just name) "TypeExport"

instance ProtoDot (ModuleExportList a) where
  toDot =
    \case
      Exports exports -> do
        id1 <- emitShape FolderShape "Exports"
        emitEdges id1 exports 
        return id1
      ExportAll ->
        emitShape FolderShape "ExportsAll"

instance (ProtoDot t, ProtoDot k, Show k, Pretty k) => ProtoDot (ProtoModule a k t) where
  toDot =
    \case
      ProtoModule{..} -> do
        id1 <- emitNamedShape RectangleShape (Just $ principalPath protoOmodulePath) "Module"
        id2 <- toDot protoOmoduleExportList
        emitEdge id1 id2
        emitEdges id1 protoOmoduleDefinitions 
        return id1

emitDefinition :: (ProtoDot t) => Text -> Text -> t -> ProtoDotGen Int
emitDefinition name label def = do
  id1 <- emitNamedShape TriangleShape (Just name) label
  id2 <- toDot def
  emitEdge id1 id2
  return id1

instance (ProtoDot t, ProtoDot k, Show k, Pretty k) => ProtoDot (ProtoDefinition a k t) where
  toDot =
    \case
      ProtoDType _ name def ->
        emitDefinition name "DType" def
      ProtoDTypeAlias _ name def ->
        emitDefinition name "DTypeAlias" def
      ProtoDFunction _ name def ->
        emitDefinition name "DFunction" def
      ProtoDLet _ name def ->
        emitDefinition name "DLet" def
      ProtoDFunctionGroup _ name defs ->
        emitNamedShape TriangleShape (Just name) "DFunctionGroup"
      ProtoDFold _ name def -> do
        emitDefinition name "DFold" def
      ProtoDImport _ path imports -> do
        id1 <- emitNamedShape TriangleShape (Just $ principalPath path) "DImport"
        _ <- foldrM connectDots id1 imports
        return id1
      ProtoDQualifiedImport _ path -> do
        emitNamedShape TriangleShape (Just $ principalPath path) "DQualifiedImport"
      ProtoDTrait _ name def ->
        emitDefinition name "DTrait" def
      ProtoDInstance _ def -> do
        id1 <- emitShape TriangleShape "DInstance"
        id2 <- toDot def
        emitEdge id1 id2
        return id1

connectDots :: (ProtoDot a) => a -> Int -> ProtoDotGen Int
connectDots a from = do
  id1 <- toDot a
  emitEdge from id1
  return id1

instance ProtoDot k => ProtoDot (ProtoTypeDefinition a k t) where
  toDot =
    \case
      ProtoTypeDefinition{..} -> do
        id1 <- emitShape EllipseShape "TypeDefinition"
        emitEdges id1 protoOtypeDefinitionParameters
        emitEdges id1 protoOtypeDefinitionConstructors
        return id1

annotation :: (ProtoDot t) => t -> ProtoDotGen Int
annotation t = do
  id1 <- emitShape RectangleShape "Annotation"
  id2 <- toDot t
  emitEdge id1 id2
  return id1

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (ProtoFunctionDefinition a k t) where
  toDot =
    \case
      ProtoFunctionDefinition{..} -> do
        id1 <- emitShape EllipseShape "FunctionDefinition"
        id2 <- annotation protoOfunctionDefinitionAnnotation
        emitEdge id1 id2
        id3 <- toDot protoOfunctionDefinitionType
        emitEdge id1 id3
        emitEdges id1 protoOfunctionDefinitionPatterns 
        id4 <- toDot protoOfunctionDefinitionExpression
        emitEdge id1 id4
        return id1

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (ProtoLetDefinition a k t) where
  toDot =
    \case
      ProtoLetDefinition{..} -> do
        id1 <- emitShape EllipseShape "LetDefinition"
        id2 <- annotation protoOletDefinitionAnnotation
        emitEdge id1 id2
        id3 <- toDot protoOletDefinitionType
        emitEdge id1 id3
        id4 <- toDot protoOletDefinitionExpression
        emitEdge id1 id4
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

instance (ProtoDot k) => ProtoDot (Parameter k) where
  toDot =
    \case
      Parameter{..} -> do
        id1 <- emitNamedShape RectangleShape (Just parameterName) "Parameter"
        id2 <- toDot parameterKind
        emitEdge id1 id2
        return id1

instance ProtoDot (Scheme o k t) where
  toDot =
    \case
      Forall{..} ->
        emitShape RectangleShape "Scheme"

instance (ProtoDot t) => ProtoDot (Trait t) where
  toDot =
    \case
      Trait{..} -> do
        id1 <- emitNamedShape HexagonShape (Just traitName) "Trait"
        id2 <- toDot traitType
        emitEdge id1 id2
        return id1

instance (ProtoDot t) => ProtoDot (With t) where
  toDot (With traits t) = do
    id1 <- emitShape HexagonShape "With"
    case traits of
      [] -> do
        id2 <- emitShape HexagonShape "[]"
        emitEdge id1 id2
      _ -> emitEdges id1 traits
    id2 <- toDot t
    emitEdge id1 id2
    return id1

instance (Show k, Pretty k) => ProtoDot (Type TypeIndex k) where
  toDot t = do
    emitShape HexagonShape (prettyType t)

instance (Show k, Pretty k) => ProtoDot (Type Parameter k) where
  toDot t = do
    emitShape HexagonShape (prettyType t)

instance ProtoDot Kind where
  toDot k = do
    emitShape HexagonShape (Text.pack $ show k)

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Binding Expression a k t) where
  toDot =
    \case
      BPattern _ p e -> do
        id1 <- emitShape RectangleShape "BPattern"
        id2 <- toDot p
        id3 <- toDot e
        emitEdge id1 id2
        emitEdge id1 id3
        return id1
      BFunction _ name ps e -> do
        id1 <- emitNamedShape RectangleShape (Just name) "BFunction"
        id2 <- toDot e
        emitEdges id1 ps
        emitEdge id1 id2
        return id1

withTypeInfo :: (ProtoDot t) => t -> ProtoDotGen Int -> ProtoDotGen Int
withTypeInfo t e = do
  id1 <- toDot t
  id2 <- e
  emitEdge id1 id2
  return id1

instance ProtoDot Primitive where
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

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Expression a k t) where
  toDot =
    \case
      EAnnotation _ t e ->
        emitShape HouseShape "EAnnotation"
      EApplication _ t e es -> do
        id1 <- withTypeInfo t $ emitShape HouseShape "EApplication"
        id2 <- toDot e
        emitEdge id1 id2
        emitEdges id1 es
        return id1
      ELambda _ ps e -> do
        id1 <- emitShape HouseShape "ELambda"
        emitEdges id1 ps
        id2 <- toDot e
        emitEdge id1 id2
        return id1
      ELet _ bs e ->
        emitShape HouseShape "ELet"
      ERecursiveLet _ p e1 e2 -> do
        id1 <- emitShape HouseShape "ERecursiveLet"
        id2 <- toDot p
        id3 <- toDot e1
        id4 <- toDot e2
        emitEdge id1 id2
        emitEdge id1 id3
        emitEdge id1 id4
        return id1
      EVariable _ (Label t name) -> do
        withTypeInfo t $ emitNamedShape HouseShape (Just name) "EVariable"
      EConstructor a ll ->
        emitShape HouseShape "EConstructor"
      ELiteral _ p -> do
        id1 <- emitShape HouseShape "ELiteral"
        id2 <- toDot p
        emitEdge id1 id2
        return id1
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
      EMatch _ t e cs -> do
        id1 <- withTypeInfo t $ emitShape HouseShape "EMatch"
        id2 <- toDot e
        emitEdge id1 id2
        emitEdges id1 cs
        return id1
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

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Pattern a k t) where
  toDot =
    \case
      PAnnotation _ t p -> do
        id1 <- withTypeInfo t $ emitShape ParallelogramShape "PAnnotation"
        id2 <- toDot p
        emitEdge id1 id2
        return id1
      PAny _ t ->
        withTypeInfo t $ emitShape ParallelogramShape "PAny"
      PVariable _ (Label t name) ->
        withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "PVariable"
      PConstructor _ (Label t name) ps -> do
        id1 <- withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "PConstructor"
        emitEdges id1 ps
        return id1
      PInteger _ t n ->
        withTypeInfo t $ emitShape ParallelogramShape "PInteger"
      PLiteral _ p -> do
        id1 <- emitShape ParallelogramShape "PLiteral"
        id2 <- toDot p
        emitEdge id1 id2
        return id1
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

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Clause a k t) where
  toDot =
    \case
      EClause{..} -> do
        id1 <- emitShape RectangleShape "EClause"
        id2 <- toDot clausePattern
        emitEdge id1 id2
        emitEdges id1 clauseChoices
        return id1

instance ProtoDot (Choice Expression a k t) where
  toDot =
    \case
      CPlain a gs e ->
        emitShape RectangleShape "CPlain"

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Guard Expression a k t) where
  toDot =
    \case
      CGuard e -> do
        id1 <- emitShape RectangleShape "CGuard"
        id2 <- toDot e
        emitEdge id1 id2
        return id1

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

labelToDotSyntax :: Text -> Maybe Text -> Text
labelToDotSyntax label Nothing = "\"" <> escapeQuotes label <> "\""
labelToDotSyntax label (Just name) = "<" <> escapeQuotes label <> "<BR/>" <> (inBold name <> "<BR/>") <> ">"

{-# INLINE escapeQuotes #-}
escapeQuotes :: Text -> Text
escapeQuotes = Text.replace "\"" "\\\""

{-# INLINE inBold #-}
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
prettyType p = Text.replace "->" "→" (renderStrict $ layoutPretty defaultLayoutOptions $ pretty p)
