{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- | This module provides utilities for generating Graphviz DOT syntax to visualize
Coal kernel language structures including modules, objects, expressions, and types.
It enables automatic graph generation for debugging compiler transformations.
-}
module Coal.Kernel.Graphviz.Dot (
  -- * Main API
  generateDotSyntax,

  -- * Core typeclass
  Dot (..),

  -- * Types
  DotNode (..),
  DotEdge (..),
  DotShape (..),

  -- * Utilities
  emitNode,
  emitEdge,
  emitShape,
) where

import Control.Monad.State (State, forM, forM_, get, modify, put, runState)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text

import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

import TextShow (showt)

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Prettyprinter.Type as KernelType
import Common (Name)

-- | Shapes available for DOT nodes
data DotShape
  = RectangleShape
  | EllipseShape
  | ParallelogramShape
  | HexagonShape
  | TriangleShape
  | DiamondShape
  | HouseShape
  | FolderShape
  | NoteShape
  deriving (Show, Eq, Read)

-- | A node in the DOT graph
data DotNode = DotNode
  { dotNodeId :: Int
  , dotNodeLabel :: Text
  , dotNodeName :: Maybe Text
  , dotNodeShape :: DotShape
  }
  deriving (Show, Eq, Read)

-- | An edge in the DOT graph
data DotEdge = DotEdge
  { dotEdgeFrom :: Int
  , dotEdgeTo :: Int
  , dotEdgeLabel :: Maybe Text
  }
  deriving (Show, Eq, Read)

-- | State for DOT graph generation
data DotState = DotState
  { dotStateSupply :: Int
  , dotStateNodes :: [DotNode]
  , dotStateEdges :: [DotEdge]
  }
  deriving (Show, Eq, Read)

-- | Insert a node into the state
insertNode :: DotNode -> DotState -> DotState
insertNode node DotState{..} =
  DotState
    { dotStateNodes = node : dotStateNodes
    , ..
    }

-- | Insert an edge into the state
insertEdge :: Int -> Int -> Maybe Text -> DotState -> DotState
insertEdge from to label DotState{..} =
  DotState
    { dotStateEdges = DotEdge from to label : dotStateEdges
    , ..
    }

-- | State monad for DOT generation
type DotGen = State DotState

-- | Generate a fresh node ID
freshId :: DotGen Int
freshId = do
  s <- get
  let newId = dotStateSupply s
  put s{dotStateSupply = newId + 1}
  return newId

-- | Emit a node to the graph
emitNode :: DotNode -> DotGen ()
emitNode = modify . insertNode

-- | Emit an edge from one node to another
emitEdge :: (Dot a) => Int -> a -> DotGen ()
emitEdge from to = do
  toId <- toDot to
  modify (insertEdge from toId Nothing)

-- | Emit an edge between two node IDs directly
emitEdgeDirect :: Int -> Int -> DotGen ()
emitEdgeDirect from to = modify (insertEdge from to Nothing)

-- | Emit an edge with a label
emitEdgeWithLabel :: (Dot a) => Text -> Int -> a -> DotGen ()
emitEdgeWithLabel label from to = do
  toId <- toDot to
  modify (insertEdge from toId (Just label))

-- | Emit multiple edges from one node to many
emitEdges :: (Traversable f, Dot a) => Int -> f a -> DotGen ()
emitEdges from tos = do
  ids <- forM tos toDot
  forM_ ids $ \toId -> modify (insertEdge from toId Nothing)

-- | Emit multiple edges with labels
emitEdgesWithLabels :: (Dot a) => [Text] -> Int -> [a] -> DotGen ()
emitEdgesWithLabels labels from tos = do
  ids <- forM tos toDot
  forM_ (zip labels ids) $ \(ll, to) ->
    modify (insertEdge from to (Just ll))

-- | Emit a shape with an optional name
emitNamedShape :: DotShape -> Maybe Text -> Text -> DotGen Int
emitNamedShape shape name label = do
  nid <- freshId
  emitNode (DotNode nid label name shape)
  return nid

-- | Emit a shape with a label
emitShape :: DotShape -> Text -> DotGen Int
emitShape shape = emitNamedShape shape Nothing

-- | Numbered labels for argument positions
numberedLabels :: [Text]
numberedLabels = ["#" <> showt i | i <- [1 :: Int ..]]

-- | Typeclass for types that can be converted to DOT graphs
class Dot a where
  toDot :: a -> DotGen Int

-- | Instance for Name (Text)
instance Dot Name where
  toDot = emitShape RectangleShape

-- | Instance for Type
instance Dot Type where
  toDot t = emitShape HexagonShape (formatTypeForDot t)

-- | Instance for Label
instance (Dot t) => Dot (Label t) where
  toDot (Label t name) = do
    nodeId <- emitNamedShape NoteShape (Just name) "Label"
    emitEdge nodeId t
    return nodeId

-- | Instance for Prim
instance Dot Prim where
  toDot =
    \case
      PUnit ->
        emitShape EllipseShape "PUnit"
      PBool b ->
        emitNamedShape EllipseShape (Just $ showt b) "PBool"
      PInt32 int ->
        emitNamedShape EllipseShape (Just $ showt int) "PInt32"
      PInt64 int ->
        emitNamedShape EllipseShape (Just $ showt int) "PInt64"
      PBignum int ->
        emitNamedShape EllipseShape (Just $ showt int) "PBignum"
      PFloat float ->
        emitNamedShape EllipseShape (Just $ showt float) "PFloat"
      PDouble double ->
        emitNamedShape EllipseShape (Just $ showt double) "PDouble"
      PChar c ->
        emitNamedShape EllipseShape (Just $ showt c) "PChar"
      PString str ->
        emitNamedShape EllipseShape (Just $ Text.pack (show str)) "PString"

-- | Helper to attach type information to a node
withTypeInfo :: (Dot t) => t -> DotGen Int -> DotGen (Int, Int)
withTypeInfo t nodeGen = do
  nodeId <- nodeGen
  typeId <- toDot t
  modify (insertEdge nodeId typeId (Just "type"))
  return (nodeId, typeId)

-- | Instance for Binding
instance (Dot t) => Dot (Binding t) where
  toDot (Binding (Label t name) expr) = do
    (bindingId, _) <- withTypeInfo t $ emitNamedShape ParallelogramShape (Just name) "Binding"
    emitEdgeWithLabel "=" bindingId expr
    return bindingId

-- | Instance for Clause
instance (Dot t) => Dot (Clause t) where
  toDot (Clause labels body) = do
    clauseId <- emitShape RectangleShape "Clause"
    emitEdges clauseId (NonEmpty.toList labels)
    emitEdgeWithLabel "→" clauseId body
    return clauseId

-- | Helper for operator emission
emitOperator :: (Dot a) => Text -> [a] -> DotGen Int
emitOperator opName operands = do
  opId <- emitShape RectangleShape opName
  emitEdgesWithLabels numberedLabels opId operands
  return opId

-- | Instance for Op
instance (Dot a) => Dot (Op a) where
  toDot =
    \case
      OEqInt32 a b -> emitOperator "=ᵢ₃₂" [a, b]
      OEqInt64 a b -> emitOperator "=ᵢ₆₄" [a, b]
      OEqFloat a b -> emitOperator "=f" [a, b]
      OEqDouble a b -> emitOperator "=d" [a, b]
      OEqChar a b -> emitOperator "=c" [a, b]
      OEqBool a b -> emitOperator "=b" [a, b]
      ONeInt32 a b -> emitOperator "≠ᵢ₃₂" [a, b]
      ONeInt64 a b -> emitOperator "≠ᵢ₆₄" [a, b]
      ONeFloat a b -> emitOperator "≠f" [a, b]
      ONeDouble a b -> emitOperator "≠d" [a, b]
      ONeChar a b -> emitOperator "≠c" [a, b]
      ONeBool a b -> emitOperator "≠b" [a, b]
      OLtInt32 a b -> emitOperator "<ᵢ₃₂" [a, b]
      OLtInt64 a b -> emitOperator "<ᵢ₆₄" [a, b]
      OLtFloat a b -> emitOperator "<f" [a, b]
      OLtDouble a b -> emitOperator "<d" [a, b]
      OGtInt32 a b -> emitOperator ">ᵢ₃₂" [a, b]
      OGtInt64 a b -> emitOperator ">ᵢ₆₄" [a, b]
      OGtFloat a b -> emitOperator ">f" [a, b]
      OGtDouble a b -> emitOperator ">d" [a, b]
      OLteInt32 a b -> emitOperator "≤ᵢ₃₂" [a, b]
      OLteInt64 a b -> emitOperator "≤ᵢ₆₄" [a, b]
      OLteFloat a b -> emitOperator "≤f" [a, b]
      OLteDouble a b -> emitOperator "≤d" [a, b]
      OGteInt32 a b -> emitOperator "≥ᵢ₃₂" [a, b]
      OGteInt64 a b -> emitOperator "≥ᵢ₆₄" [a, b]
      OGteFloat a b -> emitOperator "≥f" [a, b]
      OGteDouble a b -> emitOperator "≥d" [a, b]
      OAddInt32 a b -> emitOperator "+ᵢ₃₂" [a, b]
      OAddInt64 a b -> emitOperator "+ᵢ₆₄" [a, b]
      OAddFloat a b -> emitOperator "+f" [a, b]
      OAddDouble a b -> emitOperator "+d" [a, b]
      OSubInt32 a b -> emitOperator "−ᵢ₃₂" [a, b]
      OSubInt64 a b -> emitOperator "−ᵢ₆₄" [a, b]
      OSubFloat a b -> emitOperator "−f" [a, b]
      OSubDouble a b -> emitOperator "−d" [a, b]
      OMulInt32 a b -> emitOperator "×ᵢ₃₂" [a, b]
      OMulInt64 a b -> emitOperator "×ᵢ₆₄" [a, b]
      OMulFloat a b -> emitOperator "×f" [a, b]
      OMulDouble a b -> emitOperator "×d" [a, b]
      ODivInt32 a b -> emitOperator "÷ᵢ₃₂" [a, b]
      ODivInt64 a b -> emitOperator "÷ᵢ₆₄" [a, b]
      ODivFloat a b -> emitOperator "÷f" [a, b]
      ODivDouble a b -> emitOperator "÷d" [a, b]
      OOr a b -> emitOperator "∨" [a, b]
      OAnd a b -> emitOperator "∧" [a, b]
      ONot a -> emitOperator "¬" [a]
      ONegInt32 a -> emitOperator "−ᵢ₃₂" [a]
      ONegInt64 a -> emitOperator "−ᵢ₆₄" [a]
      ONegFloat a -> emitOperator "−f" [a]
      ONegDouble a -> emitOperator "−d" [a]

-- | Instance for Expr - the main AST visualization
instance (Dot t) => Dot (Expr t) where
  toDot =
    \case
      EVar (Label t name) -> do
        (nodeId, _) <- withTypeInfo t $ emitNamedShape RectangleShape (Just name) "EVar"
        return nodeId
      ECon (Label t name) -> do
        (nodeId, _) <- withTypeInfo t $ emitNamedShape RectangleShape (Just name) "ECon"
        return nodeId
      ELet bindings body -> do
        letId <- emitShape HouseShape "ELet"
        emitEdges letId (NonEmpty.toList bindings)
        emitEdgeWithLabel "in" letId body
        return letId
      ELit prim -> do
        litId <- emitShape EllipseShape "ELit"
        emitEdge litId prim
        return litId
      ELam params body -> do
        lamId <- emitShape ParallelogramShape "ELam"
        emitEdgesWithLabels numberedLabels lamId (NonEmpty.toList params)
        emitEdgeWithLabel "→" lamId body
        return lamId
      EApp t func args -> do
        (appId, _) <- withTypeInfo t $ emitShape DiamondShape "EApp"
        emitEdgeWithLabel "@f" appId func
        emitEdgesWithLabels numberedLabels appId (NonEmpty.toList args)
        return appId
      EIf cond thenBranch elseBranch -> do
        ifId <- emitShape DiamondShape "EIf"
        emitEdgeWithLabel "?" ifId cond
        emitEdgeWithLabel "then" ifId thenBranch
        emitEdgeWithLabel "else" ifId elseBranch
        return ifId
      EOp op -> do
        opId <- emitShape RectangleShape "EOp"
        emitEdge opId op
        return opId
      ECase t scrutinee clauses -> do
        (caseId, _) <- withTypeInfo t $ emitShape DiamondShape "ECase"
        emitEdgeWithLabel "scrutinee" caseId scrutinee
        emitEdgesWithLabels numberedLabels caseId (NonEmpty.toList clauses)
        return caseId
      EExt fieldName value rest -> do
        extId <- emitNamedShape HexagonShape (Just fieldName) "EExt"
        emitEdgeWithLabel "value" extId value
        emitEdgeWithLabel "rest" extId rest
        return extId
      ENil ->
        emitShape HexagonShape "ENil"
      EGet (Label t fieldName) record -> do
        (getId, _) <- withTypeInfo t $ emitNamedShape HexagonShape (Just fieldName) "EGet"
        emitEdgeWithLabel "from" getId record
        return getId

-- | Instance for Object
instance (Dot t) => Dot (Object t) where
  toDot =
    \case
      DFunction _ name params body -> do
        funcId <- emitNamedShape FolderShape (Just name) "DFunction"
        emitEdgesWithLabels numberedLabels funcId params
        emitEdgeWithLabel "=" funcId body
        return funcId
      DConstant name expr -> do
        constId <- emitNamedShape NoteShape (Just name) "DConstant"
        emitEdgeWithLabel "=" constId expr
        return constId
      DExternal name t -> do
        extId <- emitNamedShape TriangleShape (Just name) "DExternal"
        emitEdgeWithLabel "type" extId t
        return extId
      DData typeName ctors -> do
        dataId <- emitNamedShape TriangleShape (Just $ typeName <> " (" <> showt (length ctors) <> " ctors)") "DData"
        forM_ (zip [(0 :: Int) ..] ctors) $ \(idx, (ctorName, ctorType)) -> do
          ctorId <- emitNamedShape DiamondShape (Just $ ctorName <> "[" <> showt idx <> "]") "Constructor"
          emitEdgeDirect dataId ctorId
          emitEdgeWithLabel "type" ctorId ctorType
        return dataId

-- | Instance for Module
instance (Dot t) => Dot (Module t) where
  toDot Module{..} = do
    modId <- emitNamedShape EllipseShape (Just moduleName) "Module"
    -- Emit import edges if there are imports
    case moduleImports of
      [] -> return ()
      imports -> do
        importsId <- emitShape FolderShape "Imports"
        emitEdgeDirect modId importsId
        forM_ imports $ \imp -> do
          impId <- emitShape RectangleShape imp
          emitEdgeDirect importsId impId
    -- Emit object edges
    emitEdges modId moduleObjects
    return modId

-- | Convert a shape to DOT syntax
shapeToDotSyntax :: DotShape -> Text
shapeToDotSyntax =
  \case
    RectangleShape -> "box"
    EllipseShape -> "ellipse"
    ParallelogramShape -> "parallelogram"
    HexagonShape -> "hexagon"
    TriangleShape -> "triangle"
    DiamondShape -> "diamond"
    HouseShape -> "house"
    FolderShape -> "folder"
    NoteShape -> "note"

-- | Format edge label for DOT syntax
edgeLabelToDotSyntax :: Maybe Text -> Text
edgeLabelToDotSyntax =
  \case
    Nothing -> ""
    Just label -> " [label=\"  " <> escapeQuotes label <> "\"]"

-- | Format node label for DOT syntax
labelToDotSyntax :: Text -> Maybe Text -> Text
labelToDotSyntax label Nothing = "\"" <> escapeQuotes label <> "\""
labelToDotSyntax label (Just name) =
  "<" <> escapeHtml label <> "<BR/><B>" <> escapeHtml name <> "</B>>"

-- | Escape quotes in text
escapeQuotes :: Text -> Text
escapeQuotes = Text.replace "\"" "\\\""

-- | Escape HTML entities
escapeHtml :: Text -> Text
escapeHtml = Text.replace "<" "&lt;" . Text.replace ">" "&gt;" . Text.replace "&" "&amp;"

-- | Pretty print a Type using the kernel language pretty printer
prettyType :: Type -> Text
prettyType t = renderStrict $ layoutPretty defaultLayoutOptions $ KernelType.prettyType t

-- | Format a type for DOT display, wrapping long types across multiple lines
formatTypeForDot :: Type -> Text
formatTypeForDot t =
  let typeStr = prettyType t
      maxWidth = 40 -- Maximum characters per line before wrapping
   in if Text.length typeStr <= maxWidth
        then typeStr
        else wrapTypeString typeStr maxWidth

-- | Wrap a type string into multiple lines at natural breaking points
wrapTypeString :: Text -> Int -> Text
wrapTypeString typeStr maxWidth =
  let lines' = breakAtNaturalPoints typeStr maxWidth
   in Text.intercalate "\\l" lines' <> "\\l"

-- | Break a string at natural points (arrows, commas, row extensions)
breakAtNaturalPoints :: Text -> Int -> [Text]
breakAtNaturalPoints str maxWidth
  | Text.length str <= maxWidth = [str]
  | otherwise =
      case findBreakPoint str maxWidth of
        Just (before, after) ->
          before : breakAtNaturalPoints after maxWidth
        Nothing ->
          -- Fallback: hard break at maxWidth
          let (before, after) = Text.splitAt maxWidth str
           in before : if Text.null after then [] else breakAtNaturalPoints after maxWidth

-- | Find a good break point in a type string (prefer after '/', ',', '|')
findBreakPoint :: Text -> Int -> Maybe (Text, Text)
findBreakPoint str maxWidth =
  let prefix = Text.take (maxWidth + 20) str -- Look a bit ahead
      breakChars = ["/", " | ", ", "] -- Prefer breaking after these
      findBreak chars =
        case chars of
          [] -> Nothing
          (c : cs) ->
            case Text.breakOnEnd c prefix of
              (before, _)
                | not (Text.null before) && Text.length before <= maxWidth + Text.length c ->
                    Just (Text.dropEnd (Text.length c) before <> c, Text.drop (Text.length before) str)
                | otherwise -> findBreak cs
   in findBreak breakChars

-- | Generate DOT syntax from any Dot instance
generateDotSyntax :: (Dot a) => a -> Text
generateDotSyntax ast =
  Text.unlines $
    [ "digraph AST {"
    , "  rankdir=TB;"
    , "  node [shape=box, style=filled, fillcolor=lightblue];"
    , "  edge [arrowhead=vee];"
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
