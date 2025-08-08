{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Dotgen.ToDot (ToDot (..), writeDotFiles) where

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
import Control.Monad.RWS
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

type DotGen = RWS Int () DotState

class ToDot a where
  toDot :: a -> DotGen Int

freshNode :: Text -> DotGen Int
freshNode text = do
  nid <- supplied id
  emitNode nid text
  return nid

emitNode :: Int -> Text -> DotGen ()
emitNode nid label = modify $ \st -> st{nodes = (nid, label) : nodes st}

emitEdge :: Int -> Int -> DotGen ()
emitEdge from to = modify $ \st -> st{edges = (from, to) : edges st}

withNode :: Text -> DotGen () -> DotGen Int
withNode text f = do
  nid <- freshNode text
  local (const nid) f
  return nid

edgeTo :: (ToDot a) => a -> DotGen ()
edgeTo d = do
  from <- ask
  to <- toDot d
  emitEdge from to

edgesTo :: (Traversable t, ToDot a) => t a -> DotGen ()
edgesTo = traverse_ edgeTo

instance (ToDot a) => ToDot (Maybe a) where
  toDot =
    \case
      Nothing ->
        freshNode "Nothing"
      Just d -> do
        withNode "Just" (edgeTo d)

instance (Pretty t, Show t) => ToDot (Label t) where
  toDot =
    \case
      Label t name ->
        freshNode ("Label " <> prettyType t <> " " <> name)

instance (Pretty t, Show t) => ToDot (Expression a t) where
  toDot = 
    \case
      EAnnotation _ t inner -> do
        withNode ("EAnnotation " <> prettyType t) $
          edgeTo inner
      EApplication _ t fun args -> do
        withNode ("EApplication " <> prettyType t) $ do
          edgeTo fun
          edgesTo args
      ELambda _ patterns body -> do
        withNode "ELambda" $ do
          edgesTo patterns
          edgeTo body
      ELet _ bindings body -> do
        withNode "ELet" $ do
          forM_ bindings $
            \case
              BPattern _ pat rhs -> do
                edgeTo pat
                edgeTo rhs
              BFunction{} ->
                error "TODO"
          edgeTo body
      ERecursiveLet _ pat rhs body ->
        withNode "ERecursiveLet" $ do
          edgeTo pat
          edgeTo rhs
          edgeTo body
      EVariable _ (Label t name) ->
        freshNode ("EVariable " <> prettyType t <> " " <> name)
      EConstructor _ (Label t name) ->
        freshNode ("EConstructor " <> prettyType t <> " " <> name)
      ELiteral _ prim -> do
        freshNode ("ELiteral " <> Text.pack (show prim))
      EIf _ t e1 e2 e3 -> do
        withNode ("EIf " <> prettyType t) $
          edgesTo [e1, e2, e3]
      EUnaryOperator _ t op ->
        freshNode ("EUnaryOperator " <> Text.pack (show op) <> " " <> prettyType t)
      EBinaryOperator _ t op ->
        freshNode ("EBinaryOperator " <> Text.pack (show op) <> " " <> prettyType t)
      ERecord _ t fields tail -> do
        withNode ("ERecord " <> prettyType t) $ do
          forM_ (Map.toList fields) $
            \(name, expr) -> do
              withNode ("Field " <> name) (edgeTo expr)
          edgeTo tail
      EListCons _ t e1 e2 -> do
        withNode ("EListCons " <> prettyType t) $
          edgesTo [e1, e2]
      EListLiteral _ t es -> do
        withNode ("EListLiteral " <> prettyType t) $
          edgesTo es
      ETuple _ t es -> do
        withNode ("ETuple " <> prettyType t) $
          edgesTo es
      EMatch _ t e cs ->
        withNode ("EMatch " <> prettyType t) $ do
          edgeTo e
          edgesTo cs
      ECompiledMatch _ t e cs -> do
        withNode ("ECompiledMatch " <> prettyType t) $ do
          edgeTo e
          edgesTo cs
      EFold _ t es cs me ->
        withNode ("EFold " <> prettyType t) $ do
          edgesTo es
          edgesTo cs
          edgeTo me
      EUnfold _ t ll n ps d me -> do
        withNode ("EUnfold " <> prettyType t <> " " <> n) $ do
          edgeTo ll
          edgesTo ps
          forM_ (Map.toList d) $
            \(name, expr) -> do
              withNode ("Field " <> name) (edgeTo expr)
          edgeTo me
      ESelect _ (Label t name) e -> do
        withNode ("ESelect " <> prettyType t <> " " <> name) $
          edgeTo e
      ECodataFields{} ->
        error "TODO"
      EFocus name ll1 ll2 e1 e2 ->
        withNode ("EFocus " <> name) $ do
          edgeTo ll1
          edgeTo ll2
          edgeTo e1
          edgeTo e2
      EPlaceholder _ t tr ->
        freshNode ("EPlaceholder " <> prettyType t <> " " <> Text.pack (show tr))
      _ ->
        error "TODO"

instance (Pretty t, Show t) => ToDot (Pattern a t) where
  toDot =
    \case
      PAnnotation _ t inner ->
        withNode ("PAnnotation " <> prettyType t) $
          edgeTo inner
      PAny _ t ->
        freshNode ("PAny " <> prettyType t)
      PVariable _ (Label t name) ->
        freshNode ("PVariable " <> prettyType t <> " " <> name)
      PConstructor _ (Label t name) ps -> do
        withNode ("PConstructor " <> prettyType t <> " " <> name) $
          edgesTo ps
      PLiteral _ prim ->
        freshNode ("PLiteral " <> escapeQuotes (Text.pack (show prim)))
      PRecord _ t fields tail -> do
        withNode ("PRecord " <> prettyType t) $ do
          forM_ (Map.toList fields) $
            \(name, expr) -> do
              withNode ("Field " <> name) (edgeTo expr)
          edgeTo tail
      PListCons _ t p1 p2 ->
        withNode ("PListCons " <> prettyType t) $ do
          edgesTo [p1, p2]
      PListLiteral _ t ps -> do
        withNode ("PListLiteral " <> prettyType t) $ do
          edgesTo ps
      PTuple _ t ps -> do
        withNode ("PTuple " <> prettyType t) $
          edgesTo ps
      POr _ t p1 p2 ->
        withNode ("POr " <> prettyType t) $ do
          edgesTo [p1, p2]
      PAs _ (Label t name) p ->
        withNode ("PAs " <> prettyType t <> " " <> name) $ do
          edgeTo p
      PShorthand _ (Label t name) ->
        freshNode ("PShorthand " <> prettyType t <> " " <> name)
      PAtVariable _ (Label t name) ->
        freshNode ("PAtVariable " <> prettyType t <> " " <> name)
      PPlaceholder _ t tr ->
        freshNode ("PPlaceholder " <> prettyType t <> " " <> Text.pack (show tr))

instance (Pretty t, Show t) => ToDot (Clause a t) where
  toDot =
    \case
      EClause _ p cs ->
        withNode "EClause" $ do
          edgeTo p
          edgesTo cs

instance (Pretty t, Show t) => ToDot (Choice Expression a t) where
  toDot =
    \case
      CPlain _ gs e ->
        withNode "CPlain" $ do
          edgesTo gs
          edgeTo e
      CLambda{} ->
        error "TODO"

instance (Pretty t, Show t) => ToDot (Guard Expression a t) where
  toDot =
    \case
      CGuard e ->
        withNode "CGuard" $
          edgeTo e

instance (Pretty t, Show t) => ToDot (CompiledClause a t) where
  toDot =
    \case
      ECompiledClause lls e ->
        withNode "ECompiledClause" $ do
          edgesTo lls
          edgeTo e

instance (Show t, Pretty t) => ToDot (Definition a k t) where
  toDot =
    \case
      DFunction name (Function _ (With _ t) ps e) ->
        withNode ("DFunction " <> prettyType t <> " " <> name) $ do
          edgesTo ps
          edgeTo e
      DConstant name (Constant _ (With _ t) e) ->
        withNode ("DConstant " <> prettyType t <> " " <> name) $ do
          edgeTo e
      _ -> do
        freshNode "TODO"

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
  (_, finalState, _) = runRWS (toDot ast) 0 initialState
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
