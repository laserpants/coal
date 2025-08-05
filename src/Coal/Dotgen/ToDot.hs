{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Dotgen.ToDot where

import Coal.Language.Expression
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
