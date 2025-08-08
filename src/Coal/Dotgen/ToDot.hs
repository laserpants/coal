{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Dotgen.ToDot (
  ToDot (..),
  writeDotFiles,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (fromList1)
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

type Node = (Int, Text)
type Edge = (Int, Int)

data DotState = DotState
  { supply :: Int
  , nodes :: [Node]
  , edges :: [Edge]
  }

instance Supply DotState where
  updateSupply f DotState{..} = DotState{supply = f supply, ..}
  getSupply = supply

type DotGen = State DotState

class ToDot a where
  toDot :: a -> DotGen Int

freshId :: DotGen Int
freshId = supplied id

emitNode :: Int -> Text -> DotGen ()
emitNode nid label = modify $ \st -> st{nodes = (nid, label) : nodes st}

emitEdge :: Int -> Int -> DotGen ()
emitEdge from to = modify $ \st -> st{edges = (from, to) : edges st}

instance (ToDot a) => ToDot (Maybe a) where
  toDot =
    \case
      Nothing -> do
        nid <- freshId
        emitNode nid "Nothing"
        pure nid
      Just d -> do
        nid <- freshId
        emitNode nid "Just"
        cid <- toDot d
        emitEdge nid cid
        pure nid

instance (Pretty t) => ToDot (Label t) where
  toDot =
    \case
      Label t name -> do
        nid <- freshId
        emitNode nid ("Label" <> prettyType t <> " " <> name)
        return nid

instance (Pretty t, Show t) => ToDot (Expression a t) where
  toDot =
    \case
      EAnnotation _ t inner -> do
        nid <- freshId
        emitNode nid ("EAnnotation " <> prettyType t)
        cid <- toDot inner
        emitEdge nid cid
        return nid
      EApplication _ t fun args -> do
        nid <- freshId
        emitNode nid ("EApplication " <> prettyType t)
        fid <- toDot fun
        emitEdge nid fid
        forM_ args $
          \arg -> do
            aid <- toDot arg
            emitEdge nid aid
        return nid
      ELambda _ patterns body -> do
        nid <- freshId
        emitNode nid "ELambda"
        forM_ patterns $
          \p -> do
            pid <- toDot p
            emitEdge nid pid
        bid <- toDot body
        emitEdge nid bid
        return nid
      ELet _ bindings body -> do
        nid <- freshId
        emitNode nid "ELet"
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
        emitNode nid "ERecursiveLet"
        pid <- toDot pat
        rid <- toDot rhs
        bid <- toDot body
        emitEdge nid pid
        emitEdge nid rid
        emitEdge nid bid
        return nid
      EVariable _ (Label t name) -> do
        nid <- freshId
        emitNode nid ("EVariable " <> prettyType t <> " " <> name)
        return nid
      EConstructor _ (Label t name) -> do
        nid <- freshId
        emitNode nid ("EConstructor " <> prettyType t <> " " <> name)
        return nid
      ELiteral _ prim -> do
        nid <- freshId
        emitNode nid ("ELiteral " <> Text.pack (show prim))
        return nid
      EIf _ t e1 e2 e3 -> do
        nid <- freshId
        emitNode nid ("EIf " <> prettyType t)
        id1 <- toDot e1
        id2 <- toDot e2
        id3 <- toDot e3
        emitEdge nid id1
        emitEdge nid id2
        emitEdge nid id3
        return nid
      EUnaryOperator _ t op -> do
        nid <- freshId
        emitNode nid ("EUnaryOperator " <> Text.pack (show op) <> " " <> prettyType t)
        return nid
      EBinaryOperator _ t op -> do
        nid <- freshId
        emitNode nid ("EBinaryOperator " <> Text.pack (show op) <> " " <> prettyType t)
        return nid
      ERecord _ t fields maybeTail -> do
        nid <- freshId
        emitNode nid ("ERecord " <> prettyType t)
        -- Emit each field
        forM_ (Map.toList fields) $
          \(fieldName, fieldExpr) -> do
            fid <- freshId
            emitNode fid ("Field " <> fieldName)
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
        emitNode nid ("EListCons " <> prettyType t)
        id1 <- toDot e1
        id2 <- toDot e2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      EListLiteral _ t es -> do
        nid <- freshId
        emitNode nid ("EListLiteral " <> prettyType t)
        forM_ es $
          \e -> do
            eid <- toDot e
            emitEdge nid eid
        return nid
      ETuple _ t es -> do
        nid <- freshId
        emitNode nid ("ETuple " <> prettyType t)
        forM_ es $
          \e -> do
            eid <- toDot e
            emitEdge nid eid
        return nid
      EMatch _ t e cs -> do
        nid <- freshId
        emitNode nid ("EMatch " <> prettyType t)
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
        emitNode nid ("ECompiledMatch " <> prettyType t)
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
        emitNode nid ("EFold " <> prettyType t)
        ids1 <- traverse toDot es
        ids2 <- traverse toDot cs
        id <- toDot me
        traverse_ (emitEdge nid) ids1
        traverse_ (emitEdge nid) ids2
        emitEdge nid id
        return nid
      EUnfold _ t ll n ps d me -> do
        nid <- freshId
        emitNode nid ("EUnfold " <> prettyType t)
        error "TODO"
      ESelect _ (Label t name) e -> do
        nid <- freshId
        emitNode nid ("ESelect " <> prettyType t <> " " <> name)
        sid <- toDot e
        emitEdge nid sid
        return nid
      ECodataFields{} ->
        error "TODO"
      EFocus name ll1 ll2 e1 e2 -> do
        nid <- freshId
        emitNode nid ("EFocus " <> name)
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
        emitNode nid ("EPlaceholder " <> prettyType t)
        return nid
      _ ->
        error "TODO"

instance (Pretty t, Show t) => ToDot (Pattern a t) where
  toDot =
    \case
      PAnnotation _ t inner -> do
        nid <- freshId
        emitNode nid ("PAnnotation " <> prettyType t)
        cid <- toDot inner
        emitEdge nid cid
        return nid
      PAny _ t -> do
        nid <- freshId
        emitNode nid ("PAny " <> prettyType t)
        return nid
      PVariable _ (Label t name) -> do
        nid <- freshId
        emitNode nid ("PVariable " <> prettyType t <> " " <> name)
        return nid
      PConstructor _ (Label t name) ps -> do
        nid <- freshId
        emitNode nid ("PConstructor " <> prettyType t <> " " <> name)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      PLiteral _ prim -> do
        nid <- freshId
        emitNode nid ("PLiteral " <> escapeQuotes (Text.pack (show prim)))
        return nid
      PRecord _ t fields maybeTail -> do
        nid <- freshId
        emitNode nid ("PRecord " <> prettyType t)
        -- Emit each field
        forM_ (Map.toList fields) $
          \(fieldName, fieldExpr) -> do
            fid <- freshId
            emitNode fid ("Field " <> fieldName)
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
        emitNode nid ("PListCons " <> prettyType t)
        id1 <- toDot p1
        id2 <- toDot p2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      PListLiteral _ t ps -> do
        nid <- freshId
        emitNode nid ("PListLiteral " <> prettyType t)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      PTuple _ t ps -> do
        nid <- freshId
        emitNode nid ("PTuple " <> prettyType t)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      POr _ t p1 p2 -> do
        nid <- freshId
        emitNode nid ("POr " <> prettyType t)
        id1 <- toDot p1
        id2 <- toDot p2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      PAs _ (Label t name) p -> do
        nid <- freshId
        emitNode nid ("PAs " <> prettyType t <> " " <> name)
        id1 <- toDot p
        emitEdge nid id1
        return nid
      PShorthand _ (Label t name) -> do
        nid <- freshId
        emitNode nid ("PShorthand " <> prettyType t <> " " <> name)
        return nid
      PAtVariable _ (Label t name) -> do
        nid <- freshId
        emitNode nid ("PAtVariable " <> prettyType t <> " " <> name)
        return nid
      PPlaceholder _ t _ -> do
        nid <- freshId
        emitNode nid ("PPlaceholder " <> prettyType t)
        return nid

instance (Pretty t, Show t) => ToDot (Clause a t) where
  toDot =
    \case
      EClause _ p cs -> do
        nid <- freshId
        emitNode nid "EClause"
        id1 <- toDot p
        emitEdge nid id1
        forM_ cs $
          \c -> do
            eid <- toDot c
            emitEdge nid eid
        return nid

instance (Pretty t, Show t) => ToDot (Choice Expression a t) where
  toDot =
    \case
      CPlain _ gs e -> do
        nid <- freshId
        emitNode nid "CPlain"
        id1 <- toDot e
        emitEdge nid id1
        forM_ gs $
          \g -> do
            eid <- toDot g
            emitEdge nid eid
        return nid
      CLambda{} ->
        error "TODO"

instance (Pretty t, Show t) => ToDot (Guard Expression a t) where
  toDot =
    \case
      CGuard e -> do
        nid <- freshId
        emitNode nid "CGuard"
        cid <- toDot e
        emitEdge nid cid
        return nid

instance (Pretty t, Show t) => ToDot (CompiledClause a t) where
  toDot =
    \case
      ECompiledClause lls e -> do
        nid <- freshId
        emitNode nid "ECompiledClause"
        forM_ lls $
          \ll -> do
            cid <- toDot ll
            emitEdge nid cid
        id1 <- toDot e
        emitEdge nid id1
        return nid

instance (Show t, Pretty t) => ToDot (Definition a k t) where
  toDot =
    \case
      DFunction name (Function _ (With _ t) ps e) -> do
        nid <- freshId
        emitNode nid ("DFunction " <> prettyType t <> " " <> name)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        cid <- toDot e
        emitEdge nid cid
        return nid
      DConstant name (Constant _ (With _ t) e) -> do
        nid <- freshId
        emitNode nid ("DConstant " <> prettyType t <> " " <> name)
        cid <- toDot e
        emitEdge nid cid
        return nid
      _ -> do
        nid <- freshId
        emitNode nid "TODO"
        return nid

generateDot :: (ToDot a) => a -> Text
generateDot ast =
  Text.unlines $
    [ "digraph AST {"
    , "  node [shape=box];"
    ]
      ++ map ("  " <>) (reverse dotNodes ++ dotEdges)
      ++ ["}"]
 where
  initialState = DotState 0 [] []
  (_, finalState) = runState (toDot ast) initialState
  dotNodes = [showt nid <> " [label=\"" <> label <> "\"];" | (nid, label) <- nodes finalState]
  dotEdges = [showt from <> " -> " <> showt to <> ";" | (from, to) <- edges finalState]

{-# INLINE prettyType #-}
prettyType :: (Pretty t) => t -> Text
prettyType t = "< " <> renderPretty t <> " >"

{-# INLINE escapeQuotes #-}
escapeQuotes :: Text -> Text
escapeQuotes = Text.replace "\"" "\\\""

writeDotFile :: (ToDot a) => Text -> a -> IO ()
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
