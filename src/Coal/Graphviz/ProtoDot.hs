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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Dictionary)
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

emitEdge :: (ProtoDot a) => Int -> a -> ProtoDotGen ()
emitEdge dotId to = do
  toId <- toDot to
  modify (insertEdge dotId toId Nothing)

emitEdgeWithLabel :: (ProtoDot a) => Text -> Int -> a -> ProtoDotGen ()
emitEdgeWithLabel label dotId to = do
  toId <- toDot to
  modify (insertEdge dotId toId (Just label))

emitEdges :: (Traversable f, ProtoDot a) => Int -> f a -> ProtoDotGen ()
emitEdges from tos = do
  ids <- forM tos toDot
  forM_ ids $ emitEdge from

emitNamedShape :: DotShape -> Maybe Text -> Text -> ProtoDotGen Int
emitNamedShape shape name label = do
  nid <- freshId
  emitNode (DotNode nid label name shape)
  return nid

emitShape :: DotShape -> Text -> ProtoDotGen Int
emitShape shape = emitNamedShape shape Nothing

class ProtoDot a where
  toDot :: a -> ProtoDotGen Int

instance ProtoDot Int where
  toDot = pure

instance ProtoDot (ProtoDotGen Int) where
  toDot = id

instance (ProtoDot v) => ProtoDot (Dictionary v) where
  toDot m = do
    dotId <- emitShape RectangleShape "Dictionary"
    forM_ (Map.toList m) $
      \(k, v) -> do
        id1 <- emitNamedShape RectangleShape (Just k) "Key"
        emitEdge dotId id1
        emitEdge id1 v
    return dotId

instance (ProtoDot a) => ProtoDot (Maybe a) where
  toDot =
    \case
      Nothing -> do
        emitShape RectangleShape "Nothing"
      Just d -> do
        dotId <- emitShape RectangleShape "Just"
        emitEdge dotId d
        return dotId

instance (ProtoDot t) => ProtoDot (Label t) where
  toDot =
    \case
      Label t name -> do
        (id1, _) <- withTypeInfo t $ emitNamedShape NoteShape (Just name) "Label"
        return id1

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
        emitEdge dotId protoOmoduleExportList
        emitEdges dotId protoOmoduleDefinitions
        return dotId

emitDefinition :: (ProtoDot t) => Text -> Text -> t -> ProtoDotGen Int
emitDefinition name label def = do
  dotId <- emitNamedShape TriangleShape (Just name) label
  emitEdge dotId def
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
        emitEdge dotId def
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
  emitEdge dotId t
  return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (ProtoFunctionDefinition a k t) where
  toDot =
    \case
      ProtoFunctionDefinition{..} -> do
        dotId <- emitShape EllipseShape "FunctionDefinition"
        emitEdge dotId (annotation protoOfunctionDefinitionAnnotation)
        emitEdge dotId protoOfunctionDefinitionType
        emitEdges dotId protoOfunctionDefinitionPatterns
        emitEdge dotId protoOfunctionDefinitionExpression
        return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (ProtoLetDefinition a k t) where
  toDot =
    \case
      ProtoLetDefinition{..} -> do
        dotId <- emitShape EllipseShape "LetDefinition"
        emitEdge dotId (annotation protoOletDefinitionAnnotation)
        emitEdge dotId protoOletDefinitionType
        emitEdge dotId protoOletDefinitionExpression
        return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (ProtoFoldDefinition a k t) where
  toDot =
    \case
      ProtoFoldDefinition{..} -> do
        dotId <- emitShape EllipseShape "FoldDefinition"
        emitEdge dotId (annotation protoOfoldDefinitionAnnotation)
        emitEdges dotId protoOfoldDefinitionClauses
        return dotId

instance (ProtoDot k, Show k, Pretty k) => ProtoDot (ProtoTraitDefinition a k) where
  toDot =
    \case
      ProtoTraitDefinition{..} -> do
        dotId <- emitNamedShape EllipseShape (Just protoOtraitDefinitionTraitName) "TraitDefinition"
        emitEdges dotId protoOtraitDefinitionConstraints
        emitEdge dotId protoOtraitDefinitionParameter
        forM_ protoOtraitDefinitionInterface $
          \(name, s) -> do
            id1 <- emitNamedShape EllipseShape (Just name) "Member"
            emitEdge id1 s
        return dotId

instance (ProtoDot k, ProtoDot t, Show k, Pretty k) => ProtoDot (ProtoInstanceDefinition a k t) where
  toDot =
    \case
      ProtoInstanceDefinition{..} -> do
        dotId <- emitNamedShape EllipseShape (Just protoOinstanceDefinitionTraitName) "InstanceDefinition"
        emitEdges dotId protoOinstanceDefinitionConstraints
        emitEdge dotId protoOinstanceDefinitionType
        emitEdges dotId protoOinstanceDefinitionImplementations
        return dotId

instance (ProtoDot k, Show k, Pretty k) => ProtoDot (ProtoAliasDefinition a k) where
  toDot =
    \case
      ProtoAliasDefinition{..} -> do
        dotId <- emitShape EllipseShape "AliasDefinition"
        emitEdges dotId protoOaliasDefinitionParameters
        emitEdge dotId protoOaliasDefinitionType
        return dotId

instance (ProtoDot t, ProtoDot (o k)) => ProtoDot (DataConstructor o k t) where
  toDot =
    \case
      DataConstructor{..} -> do
        dotId <- emitNamedShape EllipseShape (Just constructorName) "DataConstructor"
        emitEdge dotId constructorScheme
        return dotId

instance (ProtoDot k) => ProtoDot (Parameter k) where
  toDot =
    \case
      Parameter{..} -> do
        (id1, _) <- withTypeInfo parameterKind $ emitNamedShape RectangleShape (Just parameterName) "Parameter"
        return id1

instance (ProtoDot t, ProtoDot (o k)) => ProtoDot (Scheme o k t) where
  toDot =
    \case
      Forall{..} -> do
        dotId <- emitShape RectangleShape "Scheme"
        emitEdges dotId (Set.toList schemeTypeVariables)
        emitEdges dotId schemeTraits
        emitEdge dotId schemeTypeBody
        return dotId

instance (ProtoDot t) => ProtoDot (Trait t) where
  toDot =
    \case
      Trait{..} -> do
        dotId <- emitNamedShape HexagonShape (Just traitName) "Trait"
        emitEdge dotId traitType
        return dotId

instance (ProtoDot t) => ProtoDot (With t) where
  toDot (With traits t) = do
    dotId <- emitShape HexagonShape "With"
    case traits of
      [] -> do
        toId <- emitShape HexagonShape "[]"
        emitEdge dotId toId
      _ -> emitEdges dotId traits
    emitEdge dotId t
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
        emitEdgeWithLabel "let" dotId p
        emitEdgeWithLabel "=" dotId e
        return dotId
      BFunction _ name ps e -> do
        dotId <- emitNamedShape RectangleShape (Just name) "BFunction"
        emitEdges dotId ps
        emitEdge dotId e
        return dotId

withTypeInfo :: (ProtoDot t) => t -> ProtoDotGen Int -> ProtoDotGen (Int, Int)
withTypeInfo t e = do
  id1 <- toDot t
  id2 <- toDot e
  emitEdge id1 id2
  return (id1, id2)

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
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EAnnotation"
        emitEdge id2 e
        return id1
      EApplication _ t e es -> do
        (id1, id2) <- withTypeInfo t $ emitShape HouseShape "EApplication"
        emitEdge id2 e
        emitEdges id2 es
        return id1
      ELambda _ ps e -> do
        dotId <- emitShape HouseShape "ELambda"
        emitEdges dotId ps
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
        emitEdges id2 cs
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

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Pattern a k t) where
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
        emitEdges id2 p1
        emitEdges id2 p2
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

instance ProtoDot Operator where
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

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Clause a k t) where
  toDot =
    \case
      EClause{..} -> do
        dotId <- emitShape RectangleShape "EClause"
        emitEdge dotId clausePattern
        emitEdges dotId clauseChoices
        return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Choice Expression a k t) where
  toDot =
    \case
      CPlain _ gs e -> do
        dotId <- emitShape RectangleShape "CPlain"
        emitEdges dotId gs
        emitEdge dotId e
        return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (Guard Expression a k t) where
  toDot =
    \case
      CGuard e -> do
        dotId <- emitShape RectangleShape "CGuard"
        emitEdge dotId e
        return dotId

instance (ProtoDot t, Show k, Pretty k) => ProtoDot (CompiledClause a k t) where
  toDot =
    \case
      ECompiledClause{..} -> do
        dotId <- emitShape RectangleShape "ECompiledClause"
        emitEdges dotId compiledClauseSegments
        emitEdge dotId compiledClauseExpression
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
