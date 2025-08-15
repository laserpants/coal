{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

-- module Coal.Dotgen.ToDot (ToDot (..), writeDotFiles) where
module Coal.Dotgen.ToDot where

import Coal.Common.Label (Label (..))
import Coal.Common.Name (Name)
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language.Expression
import Coal.Language.Expression.Binding
import Coal.Language.Expression.Choice
import Coal.Language.Module
import Coal.Language.Pattern
import Coal.Language.Trait (With (..))
import Coal.Language.Type
import Control.Monad.State
import Data.Functor.Foldable (cata)
import Data.Text (Text)
import Extra (traverse_)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import TextShow (showt)

import qualified Coal.Kernel.Language as Kernel
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
  | Note
  deriving (Show, Eq, Read)

type Node t = (Int, Text, Maybe t, Shape)
type Edge = (Int, Int, Maybe Text)

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

emitEdgeWithLabel :: Text -> Int -> Int -> DotGen t ()
emitEdgeWithLabel label from to = modify $ \st -> st{edges = (from, to, Just label) : edges st}

emitEdge :: Int -> Int -> DotGen t ()
emitEdge from to = modify $ \st -> st{edges = (from, to, Nothing) : edges st}

emitShape shape txt t = do
  nid <- freshId
  emitNode (nid, txt, t, shape)
  return nid

emitRectangle = emitShape Rectangle

emitEllipse = emitShape Ellipse

emitDiamond = emitShape Diamond

emitHexagon = emitShape Hexagon

emitNote = emitShape Note

emitParallelogram = emitShape Parallelogram

emitHouse = emitShape House

emitTriangle = emitShape Triangle

emitFields :: (ToDot t a) => Int -> [(Name, a)] -> DotGen t Int
emitFields = foldM go
 where
  go id1 (name, expr) = do
    id2 <- emitHexagon ("Field\\n" <> name) Nothing
    id3 <- toDot expr
    emitEdge id1 id2
    emitEdge id2 id3
    return id2

instance (ToDot t a) => ToDot t (Maybe a) where
  toDot =
    \case
      Nothing -> do
        emitRectangle "Nothing" Nothing
      Just d -> do
        nid <- emitRectangle "Just" Nothing
        cid <- toDot d
        emitEdge nid cid
        pure nid

instance ToDot t (Label t) where
  toDot =
    \case
      Label t name ->
        emitNote ("Label\\n" <> name) (Just t)

instance (Pretty t, Show t) => ToDot t (Expression a t) where
  toDot =
    \case
      EAnnotation _ t inner -> do
        nid <- emitRectangle ("EAnnotation\\n" <> prettyType t) Nothing
        cid <- toDot inner
        emitEdge nid cid
        return nid
      EApplication _ t fun args -> do
        nid <- emitDiamond "EApplication" (Just t)
        fid <- toDot fun
        emitEdge nid fid
        forM_ args $
          \arg -> do
            aid <- toDot arg
            emitEdge nid aid
        return nid
      ELambda _ patterns body -> do
        nid <- emitHouse "ELambda" Nothing
        forM_ patterns $
          \p -> do
            pid <- toDot p
            emitEdge nid pid
        bid <- toDot body
        emitEdge nid bid
        return nid
      ELet _ bindings body -> do
        nid <- emitRectangle "ELet" Nothing
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
        nid <- emitRectangle "ERecursiveLet" Nothing
        pid <- toDot pat
        rid <- toDot rhs
        bid <- toDot body
        emitEdge nid pid
        emitEdgeWithLabel "=" nid rid
        emitEdgeWithLabel "in" nid bid
        return nid
      EVariable _ (Label t name) ->
        emitRectangle ("EVariable\\n" <> name) (Just t)
      EConstructor _ (Label t name) ->
        emitRectangle ("EConstructor\\n" <> name) (Just t)
      ELiteral _ prim ->
        emitRectangle ("ELiteral\\n" <> Text.pack (show prim)) Nothing
      EIf _ t e1 e2 e3 -> do
        nid <- emitRectangle "EIf" (Just t)
        id1 <- toDot e1
        id2 <- toDot e2
        id3 <- toDot e3
        emitEdge nid id1
        emitEdgeWithLabel "then" nid id2
        emitEdgeWithLabel "else" nid id3
        return nid
      EUnaryOperator _ t op ->
        emitRectangle ("EUnaryOperator\\n" <> Text.pack (show op)) (Just t)
      EBinaryOperator _ t op ->
        emitRectangle ("EBinaryOperator\\n" <> Text.pack (show op)) (Just t)
      ERecord _ t fields maybeTail -> do
        nid <- emitRectangle "ERecord" (Just t)
        -- Emit each field
        zid <- emitFields nid (Map.toList fields)

        case maybeTail of
          Just tailExpr -> do
            tid <- toDot tailExpr
            emitEdge zid tid -- Record -> Tail
          Nothing -> return ()
        return nid
      EListCons _ t e1 e2 -> do
        nid <- emitRectangle "EListCons" (Just t)
        id1 <- toDot e1
        id2 <- toDot e2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      EListLiteral _ t es -> do
        nid <- emitRectangle "EListLiteral" (Just t)
        forM_ es $
          \e -> do
            eid <- toDot e
            emitEdge nid eid
        return nid
      ETuple _ t es -> do
        nid <- emitRectangle "ETuple" (Just t)
        forM_ es $
          \e -> do
            eid <- toDot e
            emitEdge nid eid
        return nid
      EMatch _ t e cs -> do
        nid <- emitRectangle "EMatch" (Just t)
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
        nid <- emitRectangle "ECompiledMatch" (Just t)
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
        nid <- emitRectangle "EFold" (Just t)
        ids1 <- traverse toDot es
        ids2 <- traverse toDot cs
        id1 <- toDot me
        traverse_ (emitEdge nid) ids1
        traverse_ (emitEdge nid) ids2
        emitEdge nid id1
        return nid
      EUnfold _ t name ps d me -> do
        nid <- emitRectangle ("EUnfold\\n" <> name) (Just t)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        forM_ (Map.toList d) $
          \(fieldName, fieldExpr) -> do
            fid <- emitHexagon ("Field\\n" <> fieldName) Nothing
            eid <- toDot fieldExpr
            emitEdge fid eid -- Field node -> Expression
            emitEdge nid fid -- Record -> Field node
        id1 <- toDot me
        emitEdge nid id1
        return nid
      ESelect _ (Label t name) e -> do
        nid <- emitRectangle ("ESelect\\n" <> name) (Just t)
        sid <- toDot e
        emitEdge nid sid
        return nid
      ECodataSelect _ (Label t name) e me -> do
        nid <- emitRectangle ("ECodataSelect\\n" <> name) (Just t)
        id1 <- toDot e
        id2 <- toDot me
        emitEdge nid id1
        emitEdge nid id2
        return nid
      ECodataFields _ t d -> do
        nid <- emitRectangle "ECodataFields" (Just t)
        void (emitFields nid (Map.toList d))
        return nid
      EFocus name ll1 ll2 e1 e2 -> do
        nid <- emitRectangle ("EFocus\\n" <> name) Nothing
        id1 <- toDot ll1
        id2 <- toDot ll2
        id3 <- toDot e1
        id4 <- toDot e2
        emitEdge nid id1
        emitEdge nid id2
        emitEdge nid id3
        emitEdge nid id4
        return nid
      EPlaceholder _ t _ ->
        emitRectangle "EPlaceholder" (Just t)

instance (Pretty t, Show t) => ToDot t (Pattern a t) where
  toDot =
    \case
      PAnnotation _ t inner -> do
        nid <- emitEllipse ("PAnnotation\\n" <> prettyType t) Nothing
        cid <- toDot inner
        emitEdge nid cid
        return nid
      PAny _ t ->
        emitEllipse "PAny" (Just t)
      PVariable _ (Label t name) ->
        emitEllipse ("PVariable\\n" <> name) (Just t)
      PConstructor _ (Label t name) ps -> do
        nid <- emitEllipse ("PConstructor\\n" <> name) (Just t)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      PLiteral _ prim ->
        emitEllipse ("PLiteral\\n" <> escapeQuotes (Text.pack (show prim))) Nothing
      PRecord _ t fields maybeTail -> do
        nid <- emitEllipse "PRecord" (Just t)
        zid <- emitFields nid (Map.toList fields)
        case maybeTail of
          Just tailExpr -> do
            tid <- toDot tailExpr
            emitEdge zid tid -- Record -> Tail
          Nothing -> return ()
        return nid
      PListCons _ t p1 p2 -> do
        nid <- emitEllipse "PListCons" (Just t)
        id1 <- toDot p1
        id2 <- toDot p2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      PListLiteral _ t ps -> do
        nid <- emitEllipse "PListLiteral" (Just t)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      PTuple _ t ps -> do
        nid <- emitEllipse "PTuple" (Just t)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        return nid
      POr _ t p1 p2 -> do
        nid <- emitEllipse "POr" (Just t)
        id1 <- toDot p1
        id2 <- toDot p2
        emitEdge nid id1
        emitEdge nid id2
        return nid
      PAs _ (Label t name) p -> do
        nid <- emitEllipse ("PAs\\n" <> name) (Just t)
        id1 <- toDot p
        emitEdge nid id1
        return nid
      PShorthand _ (Label t name) ->
        emitEllipse ("PShorthand\\n" <> name) (Just t)
      PAtVariable _ (Label t name) ->
        emitEllipse ("PAtVariable\\n" <> name) (Just t)
      PPlaceholder _ t _ ->
        emitEllipse "PPlaceholder" (Just t)

instance (Pretty t, Show t) => ToDot t (Clause a t) where
  toDot =
    \case
      EClause _ p cs -> do
        nid <- emitRectangle "EClause" Nothing
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
        nid <- emitRectangle "CPlain" Nothing
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
        nid <- emitRectangle "CGuard" Nothing
        cid <- toDot e
        emitEdge nid cid
        return nid

instance (Pretty t, Show t) => ToDot t (CompiledClause a t) where
  toDot =
    \case
      ECompiledClause lls e -> do
        nid <- emitRectangle "ECompiledClause" Nothing
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
        nid <- emitParallelogram ("DFunction\\n" <> name) (Just t)
        forM_ ps $
          \p -> do
            eid <- toDot p
            emitEdge nid eid
        cid <- toDot e
        emitEdge nid cid
        return nid
      DConstant name (Constant _ (With _ t) e) -> do
        nid <- emitParallelogram ("DConstant\\n" <> name) (Just t)
        cid <- toDot e
        emitEdge nid cid
        return nid
      DAnnotation (With ts t) d -> do
        nid <- emitParallelogram ("DAnnotation\\n" <> prettyType t) Nothing
        forM_ ts $
          \tr -> do
            tid <- emitTriangle ("Trait\\n" <> prettyType tr) Nothing
            emitEdge nid tid
        did <- toDot d
        emitEdge nid did
        return nid
      _ ->
        emitParallelogram "TODO" Nothing

instance (Show t, Pretty t) => ToDot t (Module a k t) where
  toDot =
    \case
      Module (Path path) _ ds -> do
        nid <- emitEllipse (Text.intercalate "." path) Nothing
        traverse_ toDot ds
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
  dotEdges = [showt from <> " -> " <> showt to <> renderEdgeLabel label <> ";" | (from, to, label) <- edges finalState]
  renderEdgeLabel =
    \case
      Nothing ->
        ""
      Just ll ->
        " [label=\"  " <> ll <> "\", labeldistance=2]"
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
      Note ->
        "note"

{-# INLINE prettyType #-}
prettyType :: (Pretty t) => t -> Text
prettyType p = renderStrict . layoutPretty defaultLayoutOptions $ pretty p

{-# INLINE escapeQuotes #-}
escapeQuotes :: Text -> Text
escapeQuotes = Text.replace "\"" "\\\""

writeDotFile :: (Pretty t, ToDot t a) => Text -> a -> IO ()
writeDotFile fname a = Text.writeFile ("./.debug/" <> Text.unpack fname <> ".dot") (generateDot a)

writeDotFiles :: (Pretty t, Show t) => Text -> Module a k t -> IO ()
writeDotFiles ns m@(Module (Path path) _ defs) = do
  writeDotFile prefix m
  forM_ defs $
    \case
      def@DFunction{} ->
        writeDotFile (prefixed $ definitionName def) def
      def@DConstant{} ->
        writeDotFile (prefixed $ definitionName def) def
      def@DAnnotation{} ->
        writeDotFile (prefixed $ definitionName def) def
      _ ->
        pure ()
 where
  prefix = ns <> "__" <> Text.intercalate "_" path
  prefixed n = prefix <> "_" <> n

instance ToDot Kernel.Type (Kernel.Binding Kernel.Type Int) where
  toDot =
    \case
      Kernel.Binding (Label t name) id1 -> do
        nid <- emitRectangle ("Binding\\n" <> name) (Just t)
        emitEdge nid id1
        return nid

-- TODO: ???
instance ToDot Kernel.Type (Kernel.Clause Kernel.Type (DotGen Kernel.Type Int)) where
  toDot =
    \case
      Kernel.Clause lls x -> do
        nid <- emitRectangle "Clause" Nothing
        forM_ lls $
          \ll -> do
            pid <- toDot ll
            emitEdge nid pid
        zzz <- x
        emitEdge nid zzz
        return nid

-- instance ToDot Kernel.Type (Kernel.Clause Kernel.Type Int) where

instance ToDot Kernel.Type (Kernel.Focus Kernel.Type) where
  toDot =
    \case
      Kernel.Focus name ll1 ll2 -> do
        nid <- emitRectangle ("Focus\\n" <> name) Nothing
        id1 <- toDot ll1
        id2 <- toDot ll2
        emitEdge nid id1
        emitEdge nid id2
        pure nid

emitOp txt id1 id2 = do
  nid <- emitRectangle txt Nothing
  emitEdge nid id1
  emitEdge nid id2
  pure nid

instance ToDot Kernel.Type (Kernel.Op Int) where
  toDot =
    \case
      Kernel.OEqInt32 id1 id2 ->
        emitOp "OEqInt32" id1 id2
      Kernel.OEqInt64 id1 id2 ->
        emitOp "OEqInt64" id1 id2
      Kernel.OEqFloat id1 id2 ->
        emitOp "OEqFloat" id1 id2
      Kernel.OEqDouble id1 id2 ->
        emitOp "OEqDouble" id1 id2
      Kernel.OEqChar id1 id2 ->
        emitOp "OEqChar" id1 id2
      Kernel.ONeInt32 id1 id2 ->
        emitOp "ONeInt32" id1 id2
      Kernel.ONeInt64 id1 id2 ->
        emitOp "ONeInt64" id1 id2
      Kernel.ONeFloat id1 id2 ->
        emitOp "ONeFloat" id1 id2
      Kernel.ONeDouble id1 id2 ->
        emitOp "ONeDouble" id1 id2
      Kernel.ONeChar id1 id2 ->
        emitOp "ONeChar" id1 id2
      Kernel.OLtInt32 id1 id2 ->
        emitOp "OLtInt32" id1 id2
      Kernel.OLtInt64 id1 id2 ->
        emitOp "OLtInt64" id1 id2
      Kernel.OLtFloat id1 id2 ->
        emitOp "OLtFloat" id1 id2
      Kernel.OLtDouble id1 id2 ->
        emitOp "OLtDouble" id1 id2
      Kernel.OGtInt32 id1 id2 ->
        emitOp "OGtInt32" id1 id2
      Kernel.OGtInt64 id1 id2 ->
        emitOp "OGtInt64" id1 id2
      Kernel.OGtFloat id1 id2 ->
        emitOp "OGtFloat" id1 id2
      Kernel.OGtDouble id1 id2 ->
        emitOp "OGtDouble" id1 id2
      Kernel.OLteInt32 id1 id2 ->
        emitOp "OLteInt32" id1 id2
      Kernel.OLteInt64 id1 id2 ->
        emitOp "OLteInt64" id1 id2
      Kernel.OLteFloat id1 id2 ->
        emitOp "OLteFloat" id1 id2
      Kernel.OLteDouble id1 id2 ->
        emitOp "OLteDouble" id1 id2
      Kernel.OGteInt32 id1 id2 ->
        emitOp "OGteInt32" id1 id2
      Kernel.OGteInt64 id1 id2 ->
        emitOp "OGteInt64" id1 id2
      Kernel.OGteFloat id1 id2 ->
        emitOp "OGteFloat" id1 id2
      Kernel.OGteDouble id1 id2 ->
        emitOp "OGteDouble" id1 id2
      Kernel.OAddInt32 id1 id2 ->
        emitOp "OAddInt32" id1 id2
      Kernel.OAddInt64 id1 id2 ->
        emitOp "OAddInt64" id1 id2
      Kernel.OAddFloat id1 id2 ->
        emitOp "OAddFloat" id1 id2
      Kernel.OAddDouble id1 id2 ->
        emitOp "OAddDouble" id1 id2
      Kernel.OSubInt32 id1 id2 ->
        emitOp "OSubInt32" id1 id2
      Kernel.OSubInt64 id1 id2 ->
        emitOp "OSubInt64" id1 id2
      Kernel.OSubFloat id1 id2 ->
        emitOp "OSubFloat" id1 id2
      Kernel.OSubDouble id1 id2 ->
        emitOp "OSubDouble" id1 id2
      Kernel.OMulInt32 id1 id2 ->
        emitOp "OMulInt32" id1 id2
      Kernel.OMulInt64 id1 id2 ->
        emitOp "OMulInt64" id1 id2
      Kernel.OMulFloat id1 id2 ->
        emitOp "OMulFloat" id1 id2
      Kernel.OMulDouble id1 id2 ->
        emitOp "OMulDouble" id1 id2
      Kernel.ODivInt32 id1 id2 ->
        emitOp "ODivInt32" id1 id2
      Kernel.ODivInt64 id1 id2 ->
        emitOp "ODivInt64" id1 id2
      Kernel.ODivFloat id1 id2 ->
        emitOp "ODivFloat" id1 id2
      Kernel.ODivDouble id1 id2 ->
        emitOp "ODivDouble" id1 id2
      Kernel.OOr id1 id2 ->
        emitOp "OOr" id1 id2
      Kernel.OAnd id1 id2 ->
        emitOp "OAnd" id1 id2
      Kernel.ONot id1 -> do
        nid <- emitRectangle "ONot" Nothing
        emitEdge nid id1
        pure nid

instance ToDot Kernel.Type (Kernel.Expr Kernel.Type) where
  toDot =
    cata $
      \case
        Kernel.EVar (Label t name) -> do
          emitRectangle ("EVar\\n" <> name) (Just t)
        Kernel.ELet bs e -> do
          nid <- emitRectangle "ELet" Nothing
          xs <- traverse sequence bs
          forM_ xs $
            \b -> do
              cid <- toDot b
              emitEdge nid cid
          bid <- e
          emitEdge nid bid
          return nid
        Kernel.ELit p ->
          emitRectangle ("ELit\\n" <> Text.pack (show p)) Nothing
        Kernel.ELam lls e -> do
          nid <- emitHouse "ELam" Nothing
          forM_ lls $
            \ll -> do
              pid <- toDot ll
              emitEdge nid pid
          bid <- e
          emitEdge nid bid
          return nid
        Kernel.EApp t e es -> do
          nid <- emitDiamond "EApp" (Just t)
          fid <- e
          emitEdge nid fid
          forM_ es $
            \e1 -> do
              aid <- e1
              emitEdge nid aid
          return nid
        Kernel.EIf e1 e2 e3 -> do
          nid <- emitRectangle "EIf" Nothing
          id1 <- e1
          id2 <- e2
          id3 <- e3
          emitEdge nid id1
          emitEdgeWithLabel "then" nid id2
          emitEdgeWithLabel "else" nid id3
          return nid
        Kernel.EOp op -> do
          nid <- emitRectangle "EOp\\n" Nothing
          x <- sequence op
          id1 <- toDot x
          emitEdge nid id1
          return nid
        Kernel.EMat t e cs -> do
          nid <- emitRectangle "EMat" (Just t)
          id1 <- e
          emitEdge nid id1
          forM_ cs $
            \c -> do
              pid <- toDot c
              emitEdge nid pid
          return nid
        Kernel.EExt fname e1 e2 -> do
          nid <- emitHexagon ("EExt\\n" <> fname) Nothing
          id1 <- e1
          id2 <- e2
          emitEdge nid id1
          emitEdge nid id2
          return nid
        Kernel.ENil ->
          emitHexagon "ENil" Nothing
        Kernel.ESel f e1 e2 -> do
          nid <- emitHexagon "ESel" Nothing
          cid <- toDot f
          emitEdge nid cid
          id1 <- e1
          id2 <- e2
          emitEdge nid id1
          emitEdge nid id2
          return nid
        Kernel.ECall (Label t name) es e -> do
          nid <- emitHexagon ("ECall\\n" <> name) (Just t)
          forM_ es $
            \c -> do
              cid <- c
              emitEdge nid cid
          id1 <- e
          emitEdge nid id1
          return nid
        Kernel.EMem e -> do
          nid <- emitHexagon "EMem" Nothing
          id1 <- e
          emitEdge nid id1
          return nid

instance ToDot Kernel.Type (Kernel.Object Kernel.Type (Kernel.Expr Kernel.Type)) where
  toDot =
    \case
      Kernel.OFunction name lls e -> do
        nid <- emitParallelogram ("OFunction\\n" <> name) Nothing
        forM_ lls $
          \ll -> do
            cid <- toDot ll
            emitEdge nid cid
        cid <- toDot e
        emitEdge nid cid
        return nid
      Kernel.OConstant name e -> do
        nid <- emitParallelogram ("OConstant\\n" <> name) Nothing
        cid <- toDot e
        emitEdge nid cid
        return nid
      Kernel.OExternal{} ->
        emitParallelogram "TODO" Nothing
      Kernel.OData{} ->
        emitParallelogram "TODO" Nothing

instance ToDot Kernel.Type (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)) where
  toDot =
    \case
      Kernel.Module mname _ objs -> do
        nid <- emitEllipse mname Nothing
        traverse_ toDot objs
        return nid

writeDotFilesK :: Text -> Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type) -> IO ()
writeDotFilesK ns m@(Kernel.Module mname _ _) = writeDotFile (ns <> "__" <> mname) m
