{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Dotgen.ToDot where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (NonEmpty (..), fromList1)
import Coal.Language.Expression
import Coal.Language.Expression.Binding
import Coal.Language.Pattern
import Control.Monad.State
import Data.Text (Text)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

type Node = (Int, Text)
type Edge = (Int, Int)

data DotState = DotState
  { supply :: Int
  , nodes :: [Node]
  , edges :: [Edge]
  }

type DotGen = State DotState

class ToDot a where
  toDot :: a -> DotGen Int

-- TODO: Use supply
freshId :: DotGen Int
freshId = do
  st <- get
  let nid = supply st
  put st{supply = nid + 1}
  return nid

emitNode :: Int -> Text -> DotGen ()
emitNode nid label = modify $ \st -> st{nodes = (nid, label) : nodes st}

emitEdge :: Int -> Int -> DotGen ()
emitEdge from to = modify $ \st -> st{edges = (from, to) : edges st}

instance (Show t) => ToDot (Expression a t) where
  toDot =
    \case
      EAnnotation _ t inner -> do
        nid <- freshId
        emitNode nid ("Annotation: " <> Text.pack (show t))
        cid <- toDot inner
        emitEdge nid cid
        return nid
      EApplication _ t fun args -> do
        nid <- freshId
        emitNode nid ("Application: " <> Text.pack (show t))
        fid <- toDot fun
        emitEdge nid fid
        forM_ (fromList1 args) $
          \arg -> do
            aid <- toDot arg
            emitEdge nid aid
        return nid
      ELambda _ patterns body -> do
        nid <- freshId
        emitNode nid "Lambda"
        forM_ (fromList1 patterns) $
          \p -> do
            pid <- toDot p
            emitEdge nid pid
        bid <- toDot body
        emitEdge nid bid
        return nid
      ELet _ bindings body -> do
        nid <- freshId
        emitNode nid "Let"
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
        nid <- freshId
        emitNode nid "RecursiveLet"
        pid <- toDot pat
        rid <- toDot rhs
        bid <- toDot body
        emitEdge nid pid
        emitEdge nid rid
        emitEdge nid bid
        return nid
      EVariable _ (Label _ name) -> do
        nid <- freshId
        emitNode nid ("Variable: " <> name)
        return nid
      EConstructor _ (Label _ name) -> do
        nid <- freshId
        emitNode nid ("Constructor: " <> name)
        return nid
      ELiteral _ prim -> do
        nid <- freshId
        emitNode nid ("Literal: " <> Text.pack (show prim))
        return nid
      EIf _ t e1 e2 e3 -> do
        nid <- freshId
        emitNode nid ("If: " <> Text.pack (show t))
        id1 <- toDot e1
        id2 <- toDot e2
        id3 <- toDot e3
        emitEdge nid id1
        emitEdge nid id2
        emitEdge nid id3
        return nid
      EUnaryOperator _ t op -> do
        nid <- freshId
        emitNode nid ("UnaryOperator " <> Text.pack (show op) <> ": " <> Text.pack (show t))
        return nid
      EBinaryOperator _ t op -> do
        nid <- freshId
        emitNode nid ("BinaryOperator " <> Text.pack (show op) <> ": " <> Text.pack (show t))
        return nid
      ERecord _ t fields maybeTail -> do
        nid <- freshId
        emitNode nid ("Record: " <> Text.pack (show t))
        -- Emit each field
        forM_ (Map.toList fields) $
          \(fieldName, fieldExpr) -> do
            fid <- freshId
            emitNode fid ("Field: " <> fieldName)
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
        emitNode nid ("ListCons: " <> Text.pack (show t))
        id1 <- toDot e1
        id2 <- toDot e2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      EListLiteral _ t es -> do
        nid <- freshId
        emitNode nid ("ListLiteral: " <> Text.pack (show t))
        forM_ es $
          \e -> do
            eid <- toDot e
            emitEdge nid eid
        return nid
      ETuple _ t es -> do
        nid <- freshId
        emitNode nid ("Tuple: " <> Text.pack (show t))
        forM_ es $
          \e -> do
            eid <- toDot e
            emitEdge nid eid
        return nid
      EMatch _ t e cs -> do
        nid <- freshId
        emitNode nid ("Match: " <> Text.pack (show t))
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
        nid <- freshId
        emitNode nid ("CompiledMatch: " <> Text.pack (show t))
        -- Scrutinee
        sid <- toDot e
        emitEdge nid sid
        -- Clauses
        forM_ (fromList1 cs) $
          \clause -> do
            cid <- toDot clause
            emitEdge nid cid
        return nid

instance (Show t) => ToDot (Pattern a t) where
  toDot =
    undefined

instance (Show t) => ToDot (Clause a t) where
  toDot =
    undefined

instance (Show t) => ToDot (CompiledClause a t) where
  toDot =
    undefined
