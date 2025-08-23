{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Coal.Graphviz.Dot (Dot (..), writeDotFile) where

import Coal.Common.Label (Label (..))
import Coal.Common.Name (Name)
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Language.Expression
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice
import Coal.Language.Module
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (With (..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
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

class Dot t a | a -> t where
  toDot :: a -> DotGen t Int

freshId :: DotGen t Int
freshId = supplied id

emitNode :: Node t -> DotGen t ()
emitNode node = modify $ \st -> st{nodes = node : nodes st}

emitEdgeWithLabel :: Text -> Int -> Int -> DotGen t ()
emitEdgeWithLabel label from to = modify $ \st -> st{edges = (from, to, Just label) : edges st}

emitEdge :: Int -> Int -> DotGen t ()
emitEdge from to = modify $ \st -> st{edges = (from, to, Nothing) : edges st}

emitShape :: Shape -> Text -> Maybe t -> DotGen t Int
emitShape shape txt t = do
  nid <- freshId
  emitNode (nid, txt, t, shape)
  return nid

emitRectangle
  , emitEllipse
  , emitDiamond
  , emitHexagon
  , emitNote
  , emitParallelogram
  , emitHouse
  , emitTriangle ::
    Text -> Maybe t -> DotGen t Int
emitRectangle = emitShape Rectangle
emitEllipse = emitShape Ellipse
emitDiamond = emitShape Diamond
emitHexagon = emitShape Hexagon
emitNote = emitShape Note
emitParallelogram = emitShape Parallelogram
emitHouse = emitShape House
emitTriangle = emitShape Triangle

fromNode :: DotGen t Int -> ReaderT Int (DotGen t) () -> DotGen t Int
fromNode f g = do
  nid <- f
  runReaderT g nid
  return nid

emitEdgeTo :: (Dot t a) => a -> ReaderT Int (DotGen t) ()
emitEdgeTo d = do
  nid <- ask
  id1 <- lift (toDot d)
  lift (emitEdge nid id1)

emitEdgeWithLabelTo :: (Dot t a) => Text -> a -> ReaderT Int (DotGen t) ()
emitEdgeWithLabelTo label d = do
  nid <- ask
  id1 <- lift (toDot d)
  lift (emitEdgeWithLabel label nid id1)

emitEdgeToFields :: (Dot t a) => [(Name, a)] -> ReaderT Int (DotGen t) Int
emitEdgeToFields f = do
  nid <- ask
  lift (emitFields nid f)

{-# INLINE emitEdgesTo #-}
emitEdgesTo :: (Foldable f, Dot t a) => f a -> ReaderT Int (DotGen t) ()
emitEdgesTo = traverse_ emitEdgeTo

emitFields :: (Dot t a) => Int -> [(Name, a)] -> DotGen t Int
emitFields = foldM go
 where
  go id1 (name, expr) = do
    id2 <- emitHexagon ("Field\\n" <> name) Nothing
    id3 <- toDot expr
    emitEdge id1 id2
    emitEdge id2 id3
    return id2

instance (Dot t a) => Dot t (Maybe a) where
  toDot =
    \case
      Nothing -> do
        emitRectangle "Nothing" Nothing
      Just d -> do
        nid <- emitRectangle "Just" Nothing
        cid <- toDot d
        emitEdge nid cid
        pure nid

instance Dot t (Label t) where
  toDot =
    \case
      Label t name ->
        emitNote ("Label\\n" <> name) (Just t)

instance (Pretty t, Show t) => Dot t (Binding Expression a t) where
  toDot =
    \case
      BPattern _ pat rhs -> do
        fromNode (emitRectangle "BPattern\\n" Nothing) $ do
          emitEdgeTo pat
          emitEdgeWithLabelTo "=" rhs
      BFunction{} ->
        error "TODO"

instance (Pretty t, Show t) => Dot t (Expression a t) where
  toDot =
    \case
      EAnnotation _ t inner -> do
        fromNode (emitRectangle ("EAnnotation\\n" <> prettyType t) Nothing) $ do
          emitEdgeTo inner
      EApplication _ t fun args -> do
        fromNode (emitDiamond "EApplication" (Just t)) $ do
          emitEdgeTo fun
          emitEdgesTo args
      ELambda _ patterns body -> do
        fromNode (emitHouse "ELambda" Nothing) $ do
          emitEdgesTo patterns
          emitEdgeTo body
      ELet _ bnds body -> do
        fromNode (emitRectangle "ELet" Nothing) $ do
          emitEdgesTo bnds
          emitEdgeWithLabelTo "in" body
      ERecursiveLet _ pat rhs body -> do
        fromNode (emitRectangle "ERecursiveLet" Nothing) $ do
          emitEdgeTo pat
          emitEdgeWithLabelTo "=" rhs
          emitEdgeWithLabelTo "in" body
      EVariable _ (Label t name) ->
        emitRectangle ("EVariable\\n" <> name) (Just t)
      EConstructor _ (Label t name) ->
        emitRectangle ("EConstructor\\n" <> name) (Just t)
      ELiteral _ prim ->
        emitRectangle ("ELiteral\\n" <> Text.pack (show prim)) Nothing
      EIf _ t e1 e2 e3 -> do
        fromNode (emitRectangle "EIf" (Just t)) $ do
          emitEdgeTo e1
          emitEdgeWithLabelTo "then" e2
          emitEdgeWithLabelTo "else" e3
      EUnaryOperator _ t op ->
        emitRectangle ("EUnaryOperator\\n" <> Text.pack (show op)) (Just t)
      EBinaryOperator _ t op ->
        emitRectangle ("EBinaryOperator\\n" <> Text.pack (show op)) (Just t)
      ERecord _ t fields mtail -> do
        fromNode (emitRectangle "ERecord" (Just t)) $ do
          id1 <- emitEdgeToFields (Map.toList fields)
          lift $ do
            id2 <- toDot mtail
            emitEdge id1 id2
      EListCons _ t e1 e2 ->
        fromNode (emitRectangle "EListCons" (Just t)) $ do
          emitEdgeTo e1
          emitEdgeTo e2
      EListLiteral _ t es -> do
        fromNode (emitRectangle "EListLiteral" (Just t)) $
          emitEdgesTo es
      ETuple _ t es -> do
        fromNode (emitRectangle "ETuple" (Just t)) $
          emitEdgesTo es
      EMatch _ t e cs -> do
        fromNode (emitRectangle "EMatch" (Just t)) $ do
          emitEdgeTo e
          emitEdgesTo cs
      ECompiledMatch _ t e cs -> do
        fromNode (emitRectangle "ECompiledMatch" (Just t)) $ do
          emitEdgeTo e
          emitEdgesTo cs
      EFold _ t name es cs me -> do
        fromNode (emitRectangle ("EFold\\n" <> name) (Just t)) $ do
          emitEdgesTo es
          emitEdgesTo cs
          emitEdgeTo me
      EUnfold _ t name ps d me -> do
        fromNode (emitRectangle ("EUnfold\\n" <> name) (Just t)) $ do
          emitEdgesTo ps
          void (emitEdgeToFields (Map.toList d))
          emitEdgeTo me
      ESelect _ (Label t name) e -> do
        fromNode (emitRectangle ("ESelect\\n" <> name) (Just t)) $ do
          emitEdgeTo e
      ECodataSelect _ (Label t name) e me -> do
        fromNode (emitRectangle ("ECodataSelect\\n" <> name) (Just t)) $ do
          emitEdgeTo e
          emitEdgeTo me
      ECodataFields _ t d -> do
        fromNode (emitRectangle "ECodataFields" (Just t)) $
          void (emitEdgeToFields (Map.toList d))
      EFocus name ll1 ll2 e1 e2 -> do
        fromNode (emitRectangle ("EFocus\\n" <> name) Nothing) $ do
          emitEdgeTo ll1
          emitEdgeTo ll2
          emitEdgeTo e1
          emitEdgeTo e2
      EPlaceholder _ t _ ->
        emitRectangle "EPlaceholder" (Just t)

instance (Pretty t, Show t) => Dot t (Pattern a t) where
  toDot =
    \case
      PAnnotation _ t inner -> do
        fromNode (emitEllipse ("PAnnotation\\n" <> prettyType t) Nothing) $ do
          emitEdgeTo inner
      PAny _ t ->
        emitEllipse "PAny" (Just t)
      PVariable _ (Label t name) ->
        emitEllipse ("PVariable\\n" <> name) (Just t)
      PConstructor _ (Label t name) ps -> do
        fromNode (emitEllipse ("PConstructor\\n" <> name) (Just t)) $
          emitEdgesTo ps
      PLiteral _ prim ->
        emitEllipse ("PLiteral\\n" <> escapeQuotes (Text.pack (show prim))) Nothing
      PRecord _ t fields mtail -> do
        fromNode (emitEllipse "PRecord" (Just t)) $ do
          id1 <- emitEdgeToFields (Map.toList fields)
          lift $ do
            id2 <- toDot mtail
            emitEdge id1 id2
      PListCons _ t p1 p2 -> do
        fromNode (emitEllipse "PListCons" (Just t)) $ do
          emitEdgeTo p1
          emitEdgeTo p2
      PListLiteral _ t ps -> do
        fromNode (emitEllipse "PListLiteral" (Just t)) $
          emitEdgesTo ps
      PTuple _ t ps -> do
        fromNode (emitEllipse "PTuple" (Just t)) $
          emitEdgesTo ps
      POr _ t p1 p2 -> do
        fromNode (emitEllipse "POr" (Just t)) $ do
          emitEdgeTo p1
          emitEdgeTo p2
      PAs _ (Label t name) p -> do
        fromNode (emitEllipse ("PAs\\n" <> name) (Just t)) $
          emitEdgeTo p
      PShorthand _ (Label t name) ->
        emitEllipse ("PShorthand\\n" <> name) (Just t)
      PAtVariable _ name ll ->
        fromNode (emitEllipse ("PAtVariable\\n" <> name) Nothing) $
          emitEdgeTo ll
      PPlaceholder _ t _ ->
        emitEllipse "PPlaceholder" (Just t)

instance (Pretty t, Show t) => Dot t (Clause a t) where
  toDot =
    \case
      EClause _ p cs -> do
        fromNode (emitRectangle "EClause" Nothing) $ do
          emitEdgeTo p
          emitEdgesTo cs

instance (Pretty t, Show t) => Dot t (Choice Expression a t) where
  toDot =
    \case
      CPlain _ gs e -> do
        fromNode (emitRectangle "CPlain" Nothing) $ do
          emitEdgesTo gs
          emitEdgeTo e
      CLambda{} ->
        error "TODO"

instance (Pretty t, Show t) => Dot t (Guard Expression a t) where
  toDot =
    \case
      CGuard e -> do
        fromNode (emitRectangle "CGuard" Nothing) $ do
          emitEdgeTo e

instance (Pretty t, Show t) => Dot t (CompiledClause a t) where
  toDot =
    \case
      ECompiledClause lls e -> do
        fromNode (emitRectangle "ECompiledClause" Nothing) $ do
          emitEdgesTo lls
          emitEdgeTo e

instance (Show t, Pretty t) => Dot t (Definition a k t) where
  toDot =
    \case
      DFunction name (Function _ (With _ t) ps e) -> do
        fromNode (emitParallelogram ("DFunction\\n" <> name) (Just t)) $ do
          emitEdgesTo ps
          emitEdgeTo e
      DConstant name (Constant _ (With _ t) e) -> do
        fromNode (emitParallelogram ("DConstant\\n" <> name) (Just t)) $ do
          emitEdgeTo e
      DAnnotation (With ts t) d -> do
        fromNode (emitParallelogram ("DAnnotation\\n" <> prettyType t) Nothing) $ do
          nid <- ask
          lift $ do
            forM_ ts $
              \tr -> do
                id1 <- emitTriangle ("Trait\\n" <> prettyType tr) Nothing
                emitEdge nid id1
          emitEdgeTo d
      DImport (Path _) _ ->
        emitParallelogram "DImport" Nothing
      DType name _ _ ->
        emitParallelogram ("DType\\n" <> name) Nothing
      DCodata name _ _ ->
        emitParallelogram ("DCodata\\n" <> name) Nothing
      DTrait name _ _ _ ->
        emitParallelogram ("DTrait\\n" <> name) Nothing
      DInstance name _ _ ->
        emitParallelogram ("DInstance\\n" <> name) Nothing
      _ ->
        emitParallelogram "TODO" Nothing

instance (Show t, Pretty t) => Dot t (Module a k t) where
  toDot =
    \case
      Module (Path path) _ ds -> do
        nid <- emitEllipse (Text.intercalate "." path) Nothing
        traverse_ toDot ds
        return nid

generateDot :: (Pretty t, Dot t a) => a -> Text
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
  dotNodes = [showt nid <> " [shape=" <> renderShape shape <> ", label=\"" <> escapeQuotes label <> "\\n" <> maybe "" prettyType tinfo <> "\"];" | (nid, label, tinfo, shape) <- nodes finalState]
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

writeDotFile :: (Pretty t, Dot t a) => Text -> a -> IO ()
writeDotFile fname a = Text.writeFile ("./.debug/" <> Text.unpack fname <> ".gv") (generateDot a)

instance Dot Kernel.Type (DotGen Kernel.Type Int) where
  toDot = id

instance Dot Kernel.Type (Kernel.Binding Kernel.Type (DotGen Kernel.Type Int)) where
  toDot =
    \case
      Kernel.Binding (Label t name) e -> do
        fromNode (emitRectangle ("Binding\\n" <> name) (Just t)) $ do
          emitEdgeWithLabelTo "=" e

instance Dot Kernel.Type (Kernel.Clause Kernel.Type (DotGen Kernel.Type Int)) where
  toDot =
    \case
      Kernel.Clause lls e -> do
        fromNode (emitRectangle "Clause" Nothing) $ do
          emitEdgesTo lls
          emitEdgeTo e

instance Dot Kernel.Type (Kernel.Focus Kernel.Type) where
  toDot =
    \case
      Kernel.Focus name ll1 ll2 -> do
        fromNode (emitRectangle ("Focus\\n" <> name) Nothing) $ do
          emitEdgeTo ll1
          emitEdgeTo ll2

emitOp :: (Dot t a) => Text -> [a] -> DotGen t Int
emitOp text = fromNode (emitRectangle text Nothing) . emitEdgesTo

instance Dot Kernel.Type (Kernel.Op (DotGen Kernel.Type Int)) where
  toDot =
    \case
      Kernel.OEqInt32 op1 op2 ->
        emitOp "OEqInt32" [op1, op2]
      Kernel.OEqInt64 op1 op2 ->
        emitOp "OEqInt64" [op1, op2]
      Kernel.OEqFloat op1 op2 ->
        emitOp "OEqFloat" [op1, op2]
      Kernel.OEqDouble op1 op2 ->
        emitOp "OEqDouble" [op1, op2]
      Kernel.OEqChar op1 op2 ->
        emitOp "OEqChar" [op1, op2]
      Kernel.OEqBool op1 op2 ->
        emitOp "OEqBool" [op1, op2]
      Kernel.ONeInt32 op1 op2 ->
        emitOp "ONeInt32" [op1, op2]
      Kernel.ONeInt64 op1 op2 ->
        emitOp "ONeInt64" [op1, op2]
      Kernel.ONeFloat op1 op2 ->
        emitOp "ONeFloat" [op1, op2]
      Kernel.ONeDouble op1 op2 ->
        emitOp "ONeDouble" [op1, op2]
      Kernel.ONeChar op1 op2 ->
        emitOp "ONeChar" [op1, op2]
      Kernel.ONeBool op1 op2 ->
        emitOp "ONeBool" [op1, op2]
      Kernel.OLtInt32 op1 op2 ->
        emitOp "OLtInt32" [op1, op2]
      Kernel.OLtInt64 op1 op2 ->
        emitOp "OLtInt64" [op1, op2]
      Kernel.OLtFloat op1 op2 ->
        emitOp "OLtFloat" [op1, op2]
      Kernel.OLtDouble op1 op2 ->
        emitOp "OLtDouble" [op1, op2]
      Kernel.OGtInt32 op1 op2 ->
        emitOp "OGtInt32" [op1, op2]
      Kernel.OGtInt64 op1 op2 ->
        emitOp "OGtInt64" [op1, op2]
      Kernel.OGtFloat op1 op2 ->
        emitOp "OGtFloat" [op1, op2]
      Kernel.OGtDouble op1 op2 ->
        emitOp "OGtDouble" [op1, op2]
      Kernel.OLteInt32 op1 op2 ->
        emitOp "OLteInt32" [op1, op2]
      Kernel.OLteInt64 op1 op2 ->
        emitOp "OLteInt64" [op1, op2]
      Kernel.OLteFloat op1 op2 ->
        emitOp "OLteFloat" [op1, op2]
      Kernel.OLteDouble op1 op2 ->
        emitOp "OLteDouble" [op1, op2]
      Kernel.OGteInt32 op1 op2 ->
        emitOp "OGteInt32" [op1, op2]
      Kernel.OGteInt64 op1 op2 ->
        emitOp "OGteInt64" [op1, op2]
      Kernel.OGteFloat op1 op2 ->
        emitOp "OGteFloat" [op1, op2]
      Kernel.OGteDouble op1 op2 ->
        emitOp "OGteDouble" [op1, op2]
      Kernel.OAddInt32 op1 op2 ->
        emitOp "OAddInt32" [op1, op2]
      Kernel.OAddInt64 op1 op2 ->
        emitOp "OAddInt64" [op1, op2]
      Kernel.OAddFloat op1 op2 ->
        emitOp "OAddFloat" [op1, op2]
      Kernel.OAddDouble op1 op2 ->
        emitOp "OAddDouble" [op1, op2]
      Kernel.OSubInt32 op1 op2 ->
        emitOp "OSubInt32" [op1, op2]
      Kernel.OSubInt64 op1 op2 ->
        emitOp "OSubInt64" [op1, op2]
      Kernel.OSubFloat op1 op2 ->
        emitOp "OSubFloat" [op1, op2]
      Kernel.OSubDouble op1 op2 ->
        emitOp "OSubDouble" [op1, op2]
      Kernel.OMulInt32 op1 op2 ->
        emitOp "OMulInt32" [op1, op2]
      Kernel.OMulInt64 op1 op2 ->
        emitOp "OMulInt64" [op1, op2]
      Kernel.OMulFloat op1 op2 ->
        emitOp "OMulFloat" [op1, op2]
      Kernel.OMulDouble op1 op2 ->
        emitOp "OMulDouble" [op1, op2]
      Kernel.ODivInt32 op1 op2 ->
        emitOp "ODivInt32" [op1, op2]
      Kernel.ODivInt64 op1 op2 ->
        emitOp "ODivInt64" [op1, op2]
      Kernel.ODivFloat op1 op2 ->
        emitOp "ODivFloat" [op1, op2]
      Kernel.ODivDouble op1 op2 ->
        emitOp "ODivDouble" [op1, op2]
      Kernel.OOr op1 op2 ->
        emitOp "OOr" [op1, op2]
      Kernel.OAnd op1 op2 ->
        emitOp "OAnd" [op1, op2]
      Kernel.ONot op1 -> do
        emitOp "ONot" [op1]

instance Dot Kernel.Type (Kernel.Expr Kernel.Type) where
  toDot =
    cata $
      \case
        Kernel.EVar (Label t name) -> do
          emitRectangle ("EVar\\n" <> name) (Just t)
        Kernel.ELet bs e -> do
          fromNode (emitRectangle "ELet" Nothing) $ do
            emitEdgesTo bs
            emitEdgeWithLabelTo "in" e
        Kernel.ELit p ->
          emitRectangle ("ELit\\n" <> Text.pack (show p)) Nothing
        Kernel.ELam lls e -> do
          fromNode (emitHouse "ELam" Nothing) $ do
            emitEdgesTo lls
            emitEdgeTo e
        Kernel.EApp t e es -> do
          fromNode (emitDiamond "EApp" (Just t)) $ do
            emitEdgeTo e
            emitEdgesTo es
        Kernel.EIf e1 e2 e3 -> do
          fromNode (emitRectangle "EIf" Nothing) $ do
            emitEdgeTo e1
            emitEdgeWithLabelTo "then" e2
            emitEdgeWithLabelTo "else" e3
        Kernel.EOp op -> do
          fromNode (emitRectangle "EOp\\n" Nothing) $ do
            emitEdgeTo op
        Kernel.EMat t e cs -> do
          fromNode (emitRectangle "EMat" (Just t)) $ do
            emitEdgeTo e
            emitEdgesTo cs
        Kernel.EExt fname e1 e2 -> do
          fromNode (emitHexagon ("EExt\\n" <> fname) Nothing) $ do
            emitEdgeTo e1
            emitEdgeTo e2
        Kernel.ENil ->
          emitHexagon "ENil" Nothing
        Kernel.ESel f e1 e2 -> do
          fromNode (emitHexagon "ESel" Nothing) $ do
            emitEdgeTo f
            emitEdgeTo e1
            emitEdgeTo e2
        Kernel.ECall (Label t name) es e -> do
          fromNode (emitHexagon ("ECall\\n" <> name) (Just t)) $ do
            emitEdgesTo es
            emitEdgeTo e
        Kernel.EMem e -> do
          fromNode (emitHexagon "EMem" Nothing) $ do
            emitEdgeTo e

instance Dot Kernel.Type (Kernel.Object Kernel.Type (Kernel.Expr Kernel.Type)) where
  toDot =
    \case
      Kernel.OFunction name lls e -> do
        fromNode (emitParallelogram ("OFunction\\n" <> name) Nothing) $ do
          emitEdgesTo lls
          emitEdgeTo e
      Kernel.OConstant name e -> do
        fromNode (emitParallelogram ("OConstant\\n" <> name) Nothing) $ do
          emitEdgeTo e
      Kernel.OExternal{} ->
        emitParallelogram "TODO" Nothing
      Kernel.OData{} ->
        emitParallelogram "TODO" Nothing

instance Dot Kernel.Type (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)) where
  toDot =
    \case
      Kernel.Module modn _ objs -> do
        nid <- emitEllipse modn Nothing
        traverse_ toDot objs
        return nid
