{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Coal.Dotgen.ToDot (ToDot (..), writeDotFiles) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language.Expression
import Coal.Language.Expression.Binding
import Coal.Language.Expression.Choice
import Coal.Language.Module
import Coal.Language.Pattern
import Coal.Language.Trait (With (..))
import Coal.Pretty.Type (Pretty (..), renderPretty)
import Control.Monad.State
import Data.Text (Text)
import Extra (traverse_)
import TextShow (showt)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data Shape
  = Rectangle
  | Ellipse
  | Parallelogram
  | Hexagon
  | Folder
  | Triangle
  | Diamond
  | House
  deriving (Show, Eq, Read)

type Node t = (Int, Text, Maybe t, Shape)
type Edge = (Int, Int)

data DotState t = DotState
  { supply :: Int
  , nodes :: [Node t]
  , edges :: [Edge]
  }
  deriving (Show, Eq, Read)

instance Supply (DotState t) where
  updateSupply f DotState{..} = DotState{supply = f supply, ..}
  getSupply = supply

type DotGen t = State (DotState t)

class ToDot t a | a -> t where
  toDot :: a -> DotGen t Int

freshId :: DotGen t Int
freshId = supplied id

emitNode :: Node t -> DotGen t ()
emitNode node = modify $ \st -> st{nodes = node : nodes st}

emitEdge :: Int -> Int -> DotGen t ()
emitEdge from to = modify $ \st -> st{edges = (from, to) : edges st}

instance (ToDot t a) => ToDot t (Maybe a) where
  toDot =
    \case
      Nothing -> do
        nid <- freshId
        emitNode (nid, "Nothing", Nothing, Rectangle)
        pure nid
      Just d -> do
        nid <- freshId
        emitNode (nid, "Just", Nothing, Rectangle)
        cid <- toDot d
        emitEdge nid cid
        pure nid

instance ToDot t (Label t) where
  toDot =
    \case
      Label t name -> do
        nid <- freshId
        emitNode (nid, "Label " <> name, Just t, Rectangle)
        return nid

instance (Pretty t, Show t) => ToDot t (Expression a t) where
  toDot =
    \case
      EAnnotation _ t inner -> do
        nid <- freshId
        emitNode (nid, "EAnnotation " <> prettyType t, Nothing, Rectangle)
        cid <- toDot inner
        emitEdge nid cid
        return nid
      EApplication _ t fun args -> do
        nid <- freshId
        emitNode (nid, "EApplication", Just t, Diamond)
        fid <- toDot fun
        emitEdge nid fid
        forM_ args $
          \arg -> do
            aid <- toDot arg
            emitEdge nid aid
        return nid
      ELambda _ patterns body -> do
        nid <- freshId
        emitNode (nid, "ELambda", Nothing, House)
        forM_ patterns $
          \p -> do
            pid <- toDot p
            emitEdge nid pid
        bid <- toDot body
        emitEdge nid bid
        return nid
      ELet _ bindings body -> do
        nid <- freshId
        emitNode (nid, "ELet", Nothing, Rectangle)
        forM_ bindings $
          \case
            BPattern _ pat rhs -> do
              pid <- toDot pat
              rid <- toDot rhs
              emitEdge nid pid
              emitEdge nid rid
            BFunction{} ->
              error "TODO"
        bid <- toDot body
        emitEdge nid bid
        return nid
      ERecursiveLet _ pat rhs body -> do
        nid <- freshId
        emitNode (nid, "ERecursiveLet", Nothing, Rectangle)
        pid <- toDot pat
        rid <- toDot rhs
        bid <- toDot body
        emitEdge nid pid
        emitEdge nid rid
        emitEdge nid bid
        return nid
      EVariable _ (Label t name) -> do
        nid <- freshId
        emitNode (nid, "EVariable " <> name, Just t, Rectangle)
        return nid
      EConstructor _ (Label t name) -> do
        nid <- freshId
        emitNode (nid, "EConstructor " <> name, Just t, Rectangle)
        return nid
      ELiteral _ prim -> do
        nid <- freshId
        emitNode (nid, "ELiteral " <> Text.pack (show prim), Nothing, Rectangle)
        return nid
      EIf _ t e1 e2 e3 -> do
        nid <- freshId
        emitNode (nid, "EIf", Just t, Rectangle)
        id1 <- toDot e1
        id2 <- toDot e2
        id3 <- toDot e3
        emitEdge nid id1
        emitEdge nid id2
        emitEdge nid id3
        return nid
      EUnaryOperator _ t op -> do
        nid <- freshId
        emitNode (nid, "EUnaryOperator " <> Text.pack (show op), Just t, Rectangle)
        return nid
      EBinaryOperator _ t op -> do
        nid <- freshId
        emitNode (nid, "EBinaryOperator " <> Text.pack (show op), Just t, Rectangle)
        return nid
      ERecord _ t fields maybeTail -> do
        nid <- freshId
        emitNode (nid, "ERecord", Just t, Rectangle)
        -- Emit each field
        forM_ (Map.toList fields) $
          \(fieldName, fieldExpr) -> do
            fid <- freshId
            emitNode (fid, "Field " <> fieldName, Nothing, Hexagon)
            eid <- toDot fieldExpr
            emitEdge fid eid -- Field node -> Expression
            emitEdge nid fid -- Record -> Field node
            -- Emit optional tail
        case maybeTail of
          Just tailExpr -> do
            tid <- toDot tailExpr
            emitEdge nid tid -- Record -> Tail
          Nothing -> return ()
        return nid
      EListCons _ t e1 e2 -> do
        nid <- freshId
        emitNode (nid, "EListCons", Just t, Rectangle)
        id1 <- toDot e1
        id2 <- toDot e2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      EListLiteral _ t es -> do
        nid <- freshId
        emitNode (nid, "EListLiteral", Just t, Rectangle)
        forM_ es $
          \e -> do
            eid <- toDot e
            emitEdge nid eid
        return nid
      ETuple _ t es -> do
        nid <- freshId
        emitNode (nid, "ETuple", Just t, Rectangle)
        forM_ es $
          \e -> do
            eid <- toDot e
            emitEdge nid eid
        return nid
      EMatch _ t e cs -> do
        nid <- freshId
        emitNode (nid, "EMatch", Just t, Rectangle)
        -- Scrutinee
        sid <- toDot e
        emitEdge nid sid
        -- Clauses
        forM_ cs $
          \clause -> do
            cid <- toDot clause
            emitEdge nid cid
        return nid
      ECompiledMatch _ t e cs -> do
        nid <- freshId
        emitNode (nid, "ECompiledMatch", Just t, Rectangle)
        -- Scrutinee
        sid <- toDot e
        emitEdge nid sid
        -- Clauses
        forM_ cs $
          \clause -> do
            cid <- toDot clause
            emitEdge nid cid
        return nid
      EFold _ t es cs me -> do
        nid <- freshId
        emitNode (nid, "EFold", Just t, Rectangle)
        ids1 <- traverse toDot es
        ids2 <- traverse toDot cs
        id1 <- toDot me
        traverse_ (emitEdge nid) ids1
        traverse_ (emitEdge nid) ids2
        emitEdge nid id1
        return nid
      EUnfold _ t name ps d me -> do
        nid <- freshId
        emitNode (nid, "EUnfold " <> name, Just t, Rectangle)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        forM_ (Map.toList d) $
          \(fieldName, fieldExpr) -> do
            fid <- freshId
            emitNode (fid, "Field " <> fieldName, Nothing, Hexagon)
            eid <- toDot fieldExpr
            emitEdge fid eid -- Field node -> Expression
            emitEdge nid fid -- Record -> Field node
        id1 <- toDot me
        emitEdge nid id1
        return nid
      ESelect _ (Label t name) e -> do
        nid <- freshId
        emitNode (nid, "ESelect " <> name, Just t, Rectangle)
        sid <- toDot e
        emitEdge nid sid
        return nid
      ECodataSelect _ (Label t name) e me -> do
        nid <- freshId
        emitNode (nid, "ECodataSelect " <> name, Just t, Rectangle)
        id1 <- toDot e
        id2 <- toDot me
        emitEdge nid id1
        emitEdge nid id2
        return nid
      ECodataFields _ t d -> do
        nid <- freshId
        emitNode (nid, "ECodataFields", Just t, Rectangle)
        forM_ (Map.toList d) $
          \(fieldName, fieldExpr) -> do
            fid <- freshId
            emitNode (fid, "Field " <> fieldName, Nothing, Hexagon)
            eid <- toDot fieldExpr
            emitEdge fid eid -- Field node -> Expression
            emitEdge nid fid -- Record -> Field node
        return nid
      EFocus name ll1 ll2 e1 e2 -> do
        nid <- freshId
        emitNode (nid, "EFocus " <> name, Nothing, Rectangle)
        id1 <- toDot ll1
        id2 <- toDot ll2
        id3 <- toDot e1
        id4 <- toDot e2
        emitEdge nid id1
        emitEdge nid id2
        emitEdge nid id3
        emitEdge nid id4
        return nid
      EPlaceholder _ t _ -> do
        nid <- freshId
        emitNode (nid, "EPlaceholder", Just t, Rectangle)
        return nid

instance (Pretty t, Show t) => ToDot t (Pattern a t) where
  toDot =
    \case
      PAnnotation _ t inner -> do
        nid <- freshId
        emitNode (nid, "PAnnotation " <> prettyType t, Nothing, Ellipse)
        cid <- toDot inner
        emitEdge nid cid
        return nid
      PAny _ t -> do
        nid <- freshId
        emitNode (nid, "PAny", Just t, Ellipse)
        return nid
      PVariable _ (Label t name) -> do
        nid <- freshId
        emitNode (nid, "PVariable " <> name, Just t, Ellipse)
        return nid
      PConstructor _ (Label t name) ps -> do
        nid <- freshId
        emitNode (nid, "PConstructor " <> name, Just t, Ellipse)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      PLiteral _ prim -> do
        nid <- freshId
        emitNode (nid, "PLiteral " <> escapeQuotes (Text.pack (show prim)), Nothing, Ellipse)
        return nid
      PRecord _ t fields maybeTail -> do
        nid <- freshId
        emitNode (nid, "PRecord", Just t, Ellipse)
        -- Emit each field
        forM_ (Map.toList fields) $
          \(fieldName, fieldExpr) -> do
            fid <- freshId
            emitNode (fid, "Field " <> fieldName, Nothing, Hexagon)
            eid <- toDot fieldExpr
            emitEdge fid eid -- Field node -> Pattern
            emitEdge nid fid -- Record -> Field node
            -- Emit optional tail
        case maybeTail of
          Just tailExpr -> do
            tid <- toDot tailExpr
            emitEdge nid tid -- Record -> Tail
          Nothing -> return ()
        return nid
      PListCons _ t p1 p2 -> do
        nid <- freshId
        emitNode (nid, "PListCons", Just t, Ellipse)
        id1 <- toDot p1
        id2 <- toDot p2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      PListLiteral _ t ps -> do
        nid <- freshId
        emitNode (nid, "PListLiteral", Just t, Ellipse)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      PTuple _ t ps -> do
        nid <- freshId
        emitNode (nid, "PTuple", Just t, Ellipse)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      POr _ t p1 p2 -> do
        nid <- freshId
        emitNode (nid, "POr", Just t, Ellipse)
        id1 <- toDot p1
        id2 <- toDot p2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      PAs _ (Label t name) p -> do
        nid <- freshId
        emitNode (nid, "PAs " <> name, Just t, Ellipse)
        id1 <- toDot p
        emitEdge nid id1
        return nid
      PShorthand _ (Label t name) -> do
        nid <- freshId
        emitNode (nid, "PShorthand " <> name, Just t, Ellipse)
        return nid
      PAtVariable _ (Label t name) -> do
        nid <- freshId
        emitNode (nid, "PAtVariable " <> name, Just t, Ellipse)
        return nid
      PPlaceholder _ t _ -> do
        nid <- freshId
        emitNode (nid, "PPlaceholder", Just t, Ellipse)
        return nid

instance (Pretty t, Show t) => ToDot t (Clause a t) where
  toDot =
    \case
      EClause _ p cs -> do
        nid <- freshId
        emitNode (nid, "EClause", Nothing, Rectangle)
        id1 <- toDot p
        emitEdge nid id1
        forM_ cs $
          \c -> do
            eid <- toDot c
            emitEdge nid eid
        return nid

instance (Pretty t, Show t) => ToDot t (Choice Expression a t) where
  toDot =
    \case
      CPlain _ gs e -> do
        nid <- freshId
        emitNode (nid, "CPlain", Nothing, Rectangle)
        id1 <- toDot e
        emitEdge nid id1
        forM_ gs $
          \g -> do
            eid <- toDot g
            emitEdge nid eid
        return nid
      CLambda{} ->
        error "TODO"

instance (Pretty t, Show t) => ToDot t (Guard Expression a t) where
  toDot =
    \case
      CGuard e -> do
        nid <- freshId
        emitNode (nid, "CGuard", Nothing, Rectangle)
        cid <- toDot e
        emitEdge nid cid
        return nid

instance (Pretty t, Show t) => ToDot t (CompiledClause a t) where
  toDot =
    \case
      ECompiledClause lls e -> do
        nid <- freshId
        emitNode (nid, "ECompiledClause", Nothing, Rectangle)
        forM_ lls $
          \ll -> do
            cid <- toDot ll
            emitEdge nid cid
        id1 <- toDot e
        emitEdge nid id1
        return nid

instance (Show t, Pretty t) => ToDot t (Definition a k t) where
  toDot =
    \case
      DFunction name (Function _ (With _ t) ps e) -> do
        nid <- freshId
        emitNode (nid, "DFunction " <> name, Just t, Parallelogram)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        cid <- toDot e
        emitEdge nid cid
        return nid
      DConstant name (Constant _ (With _ t) e) -> do
        nid <- freshId
        emitNode (nid, "DConstant " <> name, Just t, Parallelogram)
        cid <- toDot e
        emitEdge nid cid
        return nid
      _ -> do
        nid <- freshId
        emitNode (nid, "TODO", Nothing, Parallelogram)
        return nid

generateDot :: (Pretty t, ToDot t a) => a -> Text
generateDot ast =
  Text.unlines $
    [ "digraph AST {"
    , "  node [shape=box];"
    , "  edge [arrowhead=none];"
    ]
      ++ map ("  " <>) (reverse dotNodes ++ dotEdges)
      ++ ["}"]
 where
  initialState = DotState 0 [] []
  (_, finalState) = runState (toDot ast) initialState
  dotNodes = [showt nid <> " [shape=" <> renderShape shape <> ", label=\"" <> label <> "\\n" <> maybe "" prettyType tinfo <> "\"];" | (nid, label, tinfo, shape) <- nodes finalState]
  dotEdges = [showt from <> " -> " <> showt to <> ";" | (from, to) <- edges finalState]
  renderShape =
    \case
      Rectangle ->
        "rectangle"
      Ellipse ->
        "ellipse"
      Parallelogram ->
        "parallelogram"
      Hexagon ->
        "hexagon"
      Folder ->
        "folder"
      Triangle ->
        "triangle"
      Diamond ->
        "diamond"
      House ->
        "house"

{-# INLINE prettyType #-}
prettyType :: (Pretty t) => t -> Text
prettyType t = "< " <> renderPretty t <> " >"

{-# INLINE escapeQuotes #-}
escapeQuotes :: Text -> Text
escapeQuotes = Text.replace "\"" "\\\""

writeDotFile :: (Pretty t, ToDot t a) => Text -> a -> IO ()
writeDotFile fname a = Text.writeFile ("./.debug/" <> Text.unpack fname <> ".dot") (generateDot a)

writeDotFiles :: (Pretty t, Show t) => Text -> Module a k t -> IO ()
writeDotFiles ns (Module (Path path) _ defs) =
  forM_ defs $
    \case
      def@DFunction{} ->
        writeDotFile (prefix <> definitionName def) def
      def@DConstant{} ->
        writeDotFile (prefix <> definitionName def) def
      DAnnotation _ def ->
        writeDotFile (prefix <> definitionName def) def
      _ ->
        pure ()
 where
  prefix = ns <> "__" <> Text.intercalate "_" path <> "_"
