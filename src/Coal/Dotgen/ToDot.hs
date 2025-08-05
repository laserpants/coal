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

instance (Show t) => ToDot (Pattern a t) where
  toDot =
    undefined
