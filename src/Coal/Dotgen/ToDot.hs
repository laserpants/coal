{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Dotgen.ToDot (
  ToDot (..),
  generateDot,
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

instance (ToDot a) => ToDot (Maybe a) where
  toDot =
    \case
      Nothing ->
        freshNode "Nothing"
      Just d -> do
        nid <- freshNode "Just"
        cid <- toDot d
        emitEdge nid cid
        pure nid

freshNode :: Text -> DotGen Int
freshNode text = do
  nid <- supplied id
  emitNode nid text
  return nid

emitNode :: Int -> Text -> DotGen ()
emitNode nid label = modify $ \st -> st{nodes = (nid, label) : nodes st}

emitEdge :: Int -> Int -> DotGen ()
emitEdge from to = modify $ \st -> st{edges = (from, to) : edges st}

instance (Pretty t, Show t) => ToDot (Label t) where
  toDot =
    \case
      Label t name -> 
        freshNode ("Label " <> prettyType t <> " " <> name)

instance (Pretty t, Show t) => ToDot (Expression a t) where
  toDot =
    \case
      EAnnotation _ t inner -> do
        nid <- freshNode ("EAnnotation " <> prettyType t)
        cid <- toDot inner
        emitEdge nid cid
        return nid
      EApplication _ t fun args -> do
        nid <- freshNode ("EApplication " <> prettyType t)
        fid <- toDot fun
        emitEdge nid fid
        forM_ (fromList1 args) $
          \arg -> do
            aid <- toDot arg
            emitEdge nid aid
        return nid
      ELambda _ patterns body -> do
        nid <- freshNode "ELambda"
        forM_ (fromList1 patterns) $
          \p -> do
            pid <- toDot p
            emitEdge nid pid
        bid <- toDot body
        emitEdge nid bid
        return nid
      ELet _ bindings body -> do
        nid <- freshNode "ELet"
        forM_ (fromList1 bindings) $
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
        nid <- freshNode "ERecursiveLet"
        pid <- toDot pat
        rid <- toDot rhs
        bid <- toDot body
        emitEdge nid pid
        emitEdge nid rid
        emitEdge nid bid
        return nid
      EVariable _ (Label t name) ->
        freshNode ("EVariable " <> prettyType t <> " " <> name)
      EConstructor _ (Label t name) ->
        freshNode ("EConstructor " <> prettyType t <> " " <> name)
      ELiteral _ prim -> do
        freshNode ("ELiteral " <> Text.pack (show prim))
      EIf _ t e1 e2 e3 -> do
        nid <- freshNode ("EIf " <> prettyType t)
        id1 <- toDot e1
        id2 <- toDot e2
        id3 <- toDot e3
        emitEdge nid id1
        emitEdge nid id2
        emitEdge nid id3
        return nid
      EUnaryOperator _ t op ->
        freshNode ("EUnaryOperator " <> Text.pack (show op) <> " " <> prettyType t)
      EBinaryOperator _ t op ->
        freshNode ("EBinaryOperator " <> Text.pack (show op) <> " " <> prettyType t)
      ERecord _ t fields maybeTail -> do
        nid <- freshNode ("ERecord " <> prettyType t)
        -- Emit each field
        forM_ (Map.toList fields) $
          \(fieldName, fieldExpr) -> do
            fid <- freshNode ("Field " <> fieldName)
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
        nid <- freshNode ("EListCons " <> prettyType t)
        id1 <- toDot e1
        id2 <- toDot e2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      EListLiteral _ t es -> do
        nid <- freshNode ("EListLiteral " <> prettyType t)
        forM_ es $
          \e -> do
            eid <- toDot e
            emitEdge nid eid
        return nid
      ETuple _ t es -> do
        nid <- freshNode ("ETuple " <> prettyType t)
        forM_ es $
          \e -> do
            eid <- toDot e
            emitEdge nid eid
        return nid
      EMatch _ t e cs -> do
        nid <- freshNode ("EMatch " <> prettyType t)
        -- Scrutinee
        sid <- toDot e
        emitEdge nid sid
        -- Clauses
        forM_ (fromList1 cs) $
          \clause -> do
            cid <- toDot clause
            emitEdge nid cid
        return nid
      ECompiledMatch _ t e cs -> do
        nid <- freshNode ("ECompiledMatch " <> prettyType t)
        -- Scrutinee
        sid <- toDot e
        emitEdge nid sid
        -- Clauses
        forM_ (fromList1 cs) $
          \clause -> do
            cid <- toDot clause
            emitEdge nid cid
        return nid
      EFold _ t es cs me -> do
        nid <- freshNode ("EFold " <> prettyType t)
        ids1 <- traverse toDot es
        ids2 <- traverse toDot cs
        id <- toDot me
        traverse_ (emitEdge nid) ids1
        traverse_ (emitEdge nid) ids2
        emitEdge nid id
        return nid
      EUnfold _ t ll n ps d me -> do
        nid <- freshNode ("EUnfold " <> prettyType t)
        error "TODO"
      ESelect _ (Label t name) e -> do
        nid <- freshNode ("ESelect " <> prettyType t <> " " <> name)
        sid <- toDot e
        emitEdge nid sid
        return nid
      ECodataFields{} ->
        error "TODO"
      EFocus name ll1 ll2 e1 e2 ->
        error "TODO"
      EPlaceholder _ t tr ->
        freshNode ("EPlaceholder " <> prettyType t <> " " <> Text.pack (show tr))
      _ ->
        error "TODO"

instance (Pretty t, Show t) => ToDot (Pattern a t) where
  toDot =
    \case
      PAnnotation _ t inner -> do
        nid <- freshNode ("PAnnotation " <> prettyType t)
        cid <- toDot inner
        emitEdge nid cid
        return nid
      PAny _ t ->
        freshNode ("PAny " <> prettyType t)
      PVariable _ (Label t name) ->
        freshNode ("PVariable " <> prettyType t <> " " <> name)
      PConstructor _ (Label t name) ps -> do
        nid <- freshNode ("PConstructor " <> prettyType t <> " " <> name)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      PLiteral _ prim ->
        freshNode ("PLiteral " <> escapeQuotes (Text.pack (show prim)))
      PRecord _ t fields maybeTail -> do
        nid <- freshNode ("PRecord " <> prettyType t)
        -- Emit each field
        forM_ (Map.toList fields) $
          \(fieldName, fieldExpr) -> do
            fid <- freshNode ("Field " <> fieldName)
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
        nid <- freshNode ("PListCons " <> prettyType t)
        id1 <- toDot p1
        id2 <- toDot p2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      PListLiteral _ t ps -> do
        nid <- freshNode ("PListLiteral " <> prettyType t)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      PTuple _ t ps -> do
        nid <- freshNode ("PTuple " <> prettyType t)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      POr _ t p1 p2 -> do
        nid <- freshNode ("POr " <> prettyType t)
        id1 <- toDot p1
        id2 <- toDot p2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      PAs _ (Label t name) p -> do
        nid <- freshNode ("PAs " <> prettyType t <> " " <> name)
        id1 <- toDot p
        emitEdge nid id1
        return nid
      PShorthand _ (Label t name) ->
        freshNode ("PShorthand " <> prettyType t <> " " <> name)
      PAtVariable _ (Label t name) ->
        freshNode ("PAtVariable " <> prettyType t <> " " <> name)
      PPlaceholder _ t tr ->
        freshNode ("PPlaceholder " <> prettyType t <> " " <> Text.pack (show tr))

instance (Pretty t, Show t) => ToDot (Clause a t) where
  toDot =
    \case
      EClause _ p cs -> do
        nid <- freshNode "EClause"
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
        nid <- freshNode "CPlain"
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
        nid <- freshNode "CGuard"
        cid <- toDot e
        emitEdge nid cid
        return nid

instance (Pretty t, Show t) => ToDot (CompiledClause a t) where
  toDot =
    \case
      ECompiledClause lls e -> do
        nid <- freshNode "ECompiledClause"
        forM_ lls $
          \(Label t name) -> do
            eid <- freshNode ("Label " <> prettyType t <> " " <> name)
            emitEdge nid eid
        cid <- toDot e
        emitEdge nid cid
        return nid

instance (Show t, Pretty t) => ToDot (Definition a k t) where
  toDot =
    \case
      DFunction name (Function _ (With _ t) ps e) -> do
        nid <- freshNode ("DFunction " <> prettyType t <> " " <> name)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        cid <- toDot e
        emitEdge nid cid
        return nid
      DConstant name (Constant _ (With _ t) e) -> do
        nid <- freshNode ("DConstant " <> prettyType t <> " " <> name)
        cid <- toDot e
        emitEdge nid cid
        return nid
      _ -> do
        nid <- freshNode "TODO"
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
prettyType t = "<" <> renderPretty t <> ">"

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
