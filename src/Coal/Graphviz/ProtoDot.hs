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
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (principalPath)
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Control.Monad.State
import Data.Foldable (foldrM)
import Data.Map.Strict (Map)
import qualified Data.Set as Set
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

forEdges :: (Foldable f) => Int -> f Int -> ProtoDotGen ()
forEdges from ids = forM_ ids $ emitEdge from

emitEdge_ :: (ProtoDot a) => Int -> a -> ProtoDotGen ()
emitEdge_ dotId to = do
  toId <- toDot to
  modify (insertEdge dotId toId Nothing)

emitEdgeWithLabel :: (ProtoDot a) => Text -> Int -> a -> ProtoDotGen ()
emitEdgeWithLabel label dotId to = do
  toId <- toDot to
  modify (insertEdge dotId toId (Just label))

emitEdges :: (Traversable f, ProtoDot a) => Int -> f a -> ProtoDotGen ()
emitEdges from tos = do
  ids <- forM tos toDot
  forEdges from ids

emitNamedShape :: DotShape -> Maybe Text -> Text -> ProtoDotGen Int
emitNamedShape shape name label = do
  nid <- freshId
  emitNode (DotNode nid label name shape)
  return nid

emitShape :: DotShape -> Text -> ProtoDotGen Int
emitShape shape = emitNamedShape shape Nothing

class ProtoDot a where
  toDot :: a -> ProtoDotGen Int

instance ProtoDot (ProtoDotGen Int) where
  toDot = id

instance ProtoDot (Map k v) where
  toDot =
    undefined

instance (ProtoDot a) => ProtoDot (Maybe a) where
  toDot =
    \case
      Nothing -> do
        emitShape RectangleShape "Nothing"
      Just d -> do
        dotId <- emitShape RectangleShape "Just"
        emitEdge_ dotId d
        return dotId

instance (ProtoDot t) => ProtoDot (Label t) where
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
        dotId <- emitShape FolderShape "Exports"
        emitEdges dotId exports
        return dotId
      ExportAll ->
        emitShape FolderShape "ExportsAll"

instance (ProtoDot t, ProtoDot k, Show k, Pretty k) => ProtoDot (ProtoModule a k t) where
  toDot =
    \case
      ProtoModule{..} -> do
        dotId <- emitNamedShape RectangleShape (Just $ principalPath protoOmodulePath) "Module"
        emitEdge_ dotId protoOmoduleExportList
        emitEdges dotId protoOmoduleDefinitions
        return dotId

emitDefinition :: (ProtoDot t) => Text -> Text -> t -> ProtoDotGen Int
emitDefinition name label def = do
  dotId <- emitNamedShape TriangleShape (Just name) label
  emitEdge_ dotId def
  return dotId

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
        dotId <- emitNamedShape TriangleShape (Just $ principalPath path) "DImport"
        _ <- foldrM connectDots dotId imports
        return dotId
      ProtoDQualifiedImport _ path -> do
        emitNamedShape TriangleShape (Just $ principalPath path) "DQualifiedImport"
      ProtoDTrait _ name def ->
        emitDefinition name "DTrait" def
      ProtoDInstance _ def -> do
        dotId <- emitShape TriangleShape "DInstance"
        emitEdge_ dotId def
        return dotId

connectDots :: (ProtoDot a) => a -> Int -> ProtoDotGen Int
connectDots a from = do
  dotId <- toDot a
  emitEdge from dotId
  return dotId

instance (ProtoDot k, Show k, Pretty k) => ProtoDot (ProtoTypeDefinition a k t) where
  toDot =
    \case
      ProtoTypeDefinition{..} -> do
        dotId <- emitShape EllipseShape "TypeDefinition"
        emitEdges dotId protoOtypeDefinitionParameters
        emitEdges dotId protoOtypeDefinitionConstructors
        return dotId

annotation :: (ProtoDot t) => t -> ProtoDotGen Int
annotation t = do
  dotId <- emitShape RectangleShape "Annotation"
  emitEdge_ dotId t
  return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (ProtoFunctionDefinition a k t) where
  toDot =
    \case
      ProtoFunctionDefinition{..} -> do
        dotId <- emitShape EllipseShape "FunctionDefinition"
        emitEdge_ dotId (annotation protoOfunctionDefinitionAnnotation)
        emitEdge_ dotId protoOfunctionDefinitionType
        emitEdges dotId protoOfunctionDefinitionPatterns
        emitEdge_ dotId protoOfunctionDefinitionExpression
        return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (ProtoLetDefinition a k t) where
  toDot =
    \case
      ProtoLetDefinition{..} -> do
        dotId <- emitShape EllipseShape "LetDefinition"
        emitEdge_ dotId (annotation protoOletDefinitionAnnotation)
        emitEdge_ dotId protoOletDefinitionType
        emitEdge_ dotId protoOletDefinitionExpression
        return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (ProtoFoldDefinition a k t) where
  toDot =
    \case
      ProtoFoldDefinition{..} -> do
        dotId <- emitShape EllipseShape "FoldDefinition"
        emitEdge_ dotId (annotation protoOfoldDefinitionAnnotation)
        emitEdges dotId protoOfoldDefinitionClauses
        return dotId

instance (ProtoDot k, Show k, Pretty k) => ProtoDot (ProtoTraitDefinition a k) where
  toDot =
    \case
      ProtoTraitDefinition{..} -> do
        dotId <- emitNamedShape EllipseShape (Just protoOtraitDefinitionTraitName) "TraitDefinition"
        emitEdges dotId protoOtraitDefinitionConstraints
        emitEdge_ dotId protoOtraitDefinitionParameter
        forM_ protoOtraitDefinitionInterface $
          \(name, s) -> do
            id1 <- emitNamedShape EllipseShape (Just name) "Member"
            emitEdge_ id1 s
        return dotId

instance (ProtoDot k, ProtoDot t, Show k, Pretty k) => ProtoDot (ProtoInstanceDefinition a k t) where
  toDot =
    \case
      ProtoInstanceDefinition{..} -> do
        dotId <- emitNamedShape EllipseShape (Just protoOinstanceDefinitionTraitName) "InstanceDefinition"
        emitEdges dotId protoOinstanceDefinitionConstraints
        emitEdge_ dotId protoOinstanceDefinitionType
        emitEdges dotId protoOinstanceDefinitionImplementations
        return dotId

instance (ProtoDot k, Show k, Pretty k) => ProtoDot (ProtoAliasDefinition a k) where
  toDot =
    \case
      ProtoAliasDefinition{..} -> do
        dotId <- emitShape EllipseShape "AliasDefinition"
        emitEdges dotId protoOaliasDefinitionParameters
        emitEdge_ dotId protoOaliasDefinitionType
        return dotId

instance (ProtoDot t, ProtoDot (o k)) => ProtoDot (DataConstructor o k t) where
  toDot =
    \case
      DataConstructor{..} -> do
        dotId <- emitNamedShape EllipseShape (Just constructorName) "DataConstructor"
        emitEdge_ dotId constructorScheme
        return dotId

instance (ProtoDot k) => ProtoDot (Parameter k) where
  toDot =
    \case
      Parameter{..} -> do
        dotId <- emitNamedShape RectangleShape (Just parameterName) "Parameter"
        emitEdge_ dotId parameterKind
        return dotId

instance (ProtoDot t, ProtoDot (o k)) => ProtoDot (Scheme o k t) where
  toDot =
    \case
      Forall{..} -> do
        dotId <- emitShape RectangleShape "Scheme"
        emitEdges dotId (Set.toList schemeTypeVariables)
        emitEdges dotId schemeTraits
        emitEdge_ dotId schemeTypeBody
        return dotId

instance (ProtoDot t) => ProtoDot (Trait t) where
  toDot =
    \case
      Trait{..} -> do
        dotId <- emitNamedShape HexagonShape (Just traitName) "Trait"
        emitEdge_ dotId traitType
        return dotId

instance (ProtoDot t) => ProtoDot (With t) where
  toDot (With traits t) = do
    dotId <- emitShape HexagonShape "With"
    case traits of
      [] -> do
        toId <- emitShape HexagonShape "[]"
        emitEdge dotId toId
      _ -> emitEdges dotId traits
    emitEdge_ dotId t
    return dotId

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
        dotId <- emitShape RectangleShape "BPattern"
        emitEdge_ dotId p
        emitEdge_ dotId e
        return dotId
      BFunction _ name ps e -> do
        dotId <- emitNamedShape RectangleShape (Just name) "BFunction"
        emitEdges dotId ps
        emitEdge_ dotId e
        return dotId

withTypeInfo :: (ProtoDot t) => t -> ProtoDotGen Int -> ProtoDotGen Int
withTypeInfo t e = do
  dotId <- toDot t
  emitEdge_ dotId e
  return dotId

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
      EAnnotation _ t e -> do
        dotId <- withTypeInfo t $ emitShape HouseShape "EAnnotation"
        emitEdge_ dotId e
        return dotId
      EApplication _ t e es -> do
        dotId <- withTypeInfo t $ emitShape HouseShape "EApplication"
        emitEdge_ dotId e
        emitEdges dotId es
        return dotId
      ELambda _ ps e -> do
        dotId <- emitShape HouseShape "ELambda"
        emitEdges dotId ps
        emitEdge_ dotId e
        return dotId
      ELet _ bs e -> do
        dotId <- emitShape HouseShape "ELet"
        emitEdges dotId bs
        emitEdge_ dotId e
        return dotId
      ERecursiveLet _ p e1 e2 -> do
        dotId <- emitShape HouseShape "ERecursiveLet"
        emitEdge_ dotId p
        emitEdge_ dotId e1
        emitEdge_ dotId e2
        return dotId
      EVariable _ (Label t name) -> do
        withTypeInfo t $ emitNamedShape HouseShape (Just name) "EVariable"
      EConstructor _ (Label t name) ->
        withTypeInfo t $ emitNamedShape HouseShape (Just name) "EConstructor"
      ELiteral _ p -> do
        dotId <- emitShape HouseShape "ELiteral"
        emitEdge_ dotId p
        return dotId
      EIf _ t e1 e2 e3 -> do
        dotId <- withTypeInfo t $ emitShape HouseShape "EIf"
        emitEdge_ dotId e1
        emitEdgeWithLabel "then" dotId e2
        emitEdgeWithLabel "else" dotId e3
        return dotId
      EOperator _ t op -> do
        dotId <- withTypeInfo t $ emitShape HouseShape "EOperator"
        emitEdge_ dotId op
        return dotId
      ERecord _ t d me -> do
        dotId <- withTypeInfo t $ emitShape HouseShape "ERecord"
        emitEdge_ dotId d
        emitEdge_ dotId me
        return dotId
      EListCons _ t e1 e2 -> do
        dotId <- withTypeInfo t $ emitShape HouseShape "EListCons"
        emitEdge_ dotId e1
        emitEdge_ dotId e2
        return dotId
      EListLiteral _ t es -> do
        dotId <- withTypeInfo t $ emitShape HouseShape "EListLiteral"
        emitEdges dotId es
        return dotId
      ETuple _ t es -> do
        dotId <- withTypeInfo t $ emitShape HouseShape "ETuple"
        emitEdges dotId es
        return dotId
      EMatch _ t e cs -> do
        dotId <- withTypeInfo t $ emitShape HouseShape "EMatch"
        emitEdge_ dotId e
        emitEdges dotId cs
        return dotId
      ELambdaMatch _ t cs -> do
        dotId <- withTypeInfo t $ emitShape HouseShape "ELambdaMatch"
        emitEdges dotId cs
        return dotId
      ECompiledMatch _ t e cs ->
        emitShape HouseShape "ECompiledMatch"
      EFold _ t es cs ->
        emitShape HouseShape "EFold"
      ESelect _ ll e ->
        emitShape HouseShape "ESelect"
      EFocus _ name ll1 ll2 e1 e2 ->
        emitShape HouseShape "EFocus"
      ETraitInstance _ t tr ->
        emitShape HouseShape "ETraitInstance"
      EFFICall _ t ll es e ->
        emitShape HouseShape "EFFICall"
      EDoBlock _ is ->
        emitShape HouseShape "EDoBlock"

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Pattern a k t) where
  toDot =
    \case
      PAnnotation _ t p -> do
        dotId <- withTypeInfo t $ emitShape ParallelogramShape "PAnnotation"
        emitEdge_ dotId p
        return dotId
      PAny _ t ->
        withTypeInfo t $ emitShape ParallelogramShape "PAny"
      PVariable _ (Label t name) ->
        withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "PVariable"
      PConstructor _ (Label t name) ps -> do
        dotId <- withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "PConstructor"
        emitEdges dotId ps
        return dotId
      PInteger _ t n ->
        withTypeInfo t $ emitShape ParallelogramShape "PInteger"
      PLiteral _ p -> do
        dotId <- emitShape ParallelogramShape "PLiteral"
        emitEdge_ dotId p
        return dotId
      PRecord _ t d mp -> do
        dotId <- withTypeInfo t $ emitShape ParallelogramShape "PRecord"
        emitEdge_ dotId d
        emitEdge_ dotId mp
        return dotId
      PListCons _ t p1 p2 -> do
        dotId <- withTypeInfo t $ emitShape ParallelogramShape "PListCons"
        emitEdges dotId p1
        emitEdges dotId p2
        return dotId
      PListLiteral _ t ps -> do
        dotId <- withTypeInfo t $ emitShape ParallelogramShape "PListLiteral"
        emitEdges dotId ps
        return dotId
      PTuple _ t ps -> do
        dotId <- withTypeInfo t $ emitShape ParallelogramShape "PTuple"
        emitEdges dotId ps
        return dotId
      POr _ t p1 p2 -> do
        dotId <- withTypeInfo t $ emitShape ParallelogramShape "POr"
        emitEdges dotId p1
        emitEdges dotId p2
        return dotId
      PAs _ (Label t name) p -> do
        dotId <- withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "PAs"
        emitEdge_ dotId p
        return dotId
      PShorthand _ ll ->
        emitShape ParallelogramShape "PShorthand"
      PAtVariable _ ll ->
        emitShape ParallelogramShape "PAtVariable"
      PNamedFold _ name ll ->
        emitShape ParallelogramShape "PNamedFold"
      PTraitInstance _ t tr ->
        emitShape ParallelogramShape "PTraitInstance"

instance ProtoDot Operator where
  toDot =
    undefined

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Clause a k t) where
  toDot =
    \case
      EClause{..} -> do
        dotId <- emitShape RectangleShape "EClause"
        emitEdge_ dotId clausePattern
        emitEdges dotId clauseChoices
        return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Choice Expression a k t) where
  toDot =
    \case
      CPlain _ gs e -> do
        dotId <- emitShape RectangleShape "CPlain"
        emitEdges dotId gs
        emitEdge_ dotId e
        return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Guard Expression a k t) where
  toDot =
    \case
      CGuard e -> do
        dotId <- emitShape RectangleShape "CGuard"
        emitEdge_ dotId e
        return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (CompiledClause a k t) where
  toDot =
    \case
      ECompiledClause{..} -> do
        dotId <- emitShape RectangleShape "ECompiledClause"
        emitEdges dotId compiledClauseSegments
        emitEdge_ dotId compiledClauseExpression
        return dotId

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
labelToDotSyntax label (Just name) = "<" <> escapeQuotes label <> "<BR/>" <> inBold name <> "<BR/>" <> ">"

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
