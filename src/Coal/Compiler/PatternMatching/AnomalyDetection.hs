{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.PatternMatching.AnomalyDetection (
  Pat (..),
  exhaustive,
  translatePattern,
) where

import Coal.Common.Environment (Environment (..), mapEnvironment)
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Stack
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Primitive (Primitive (..))
import Control.Monad.Extra (anyM, (||^))
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Extras (Name, (<$$>))
import TextShow (showt)

anomaliesEnvironment :: [(Name, [Name])] -> Environment (Set Name)
anomaliesEnvironment = Environment.fromList . (Set.fromList <$$>)

data Pat
  = Con Name [Pat]
  | Lit Primitive
  | Or Pat Pat
  | Rec [(Name, Pat)] (Maybe Pat)
  | Any
  deriving (Show, Eq, Ord, Read)

exhaustive :: (Monad m) => [Pat] -> CompilerT a m Bool
exhaustive ps = not <$> isUseful ((: []) <$> ps) [Any]

specialized :: Name -> Int -> [[Pat]] -> [[Pat]]
specialized name a = concatMap go
 where
  go [] =
    error "Implementation error"
  go (p : ps) =
    case p of
      Rec{} ->
        []
      Con name' rs
        | name' == name -> [rs <> ps]
        | otherwise -> []
      Lit l ->
        go (Con (prim l) [] : ps)
      Or r1 r2 ->
        specialized name a [r1 : ps, r2 : ps]
      _ ->
        [replicate a Any <> ps]

defaultMatrix :: [[Pat]] -> [[Pat]]
defaultMatrix = concatMap go
 where
  go (p : ps) =
    case p of
      Rec{} ->
        []
      Con{} ->
        []
      Lit{} ->
        []
      Or r1 r2 ->
        [r1 : ps, r2 : ps]
      _ ->
        [ps]
  go [] =
    error "Implementation error"

headCons :: [[Pat]] -> [(Name, Int)]
headCons = concatMap go
 where
  go [] = error "Implementation error"
  go ps =
    case head ps of
      Rec{} ->
        []
      Lit p ->
        [(prim p, 0)]
      Con name rs ->
        [(name, length rs)]
      Or r1 r2 ->
        go [r1] <> go [r2]
      _ ->
        []

prim :: Primitive -> Name
prim =
  \case
    LUnit ->
      "%()"
    LBool True ->
      "%True"
    LBool False ->
      "%False"
    LInt32{} ->
      "%Int32"
    LInt64{} ->
      "%Int64"
    LBignum{} ->
      "%Bignum"
    LFloat{} ->
      "%Float"
    LDouble{} ->
      "%Double"
    LChar{} ->
      "%Char"
    LString{} ->
      "%String"

isUseful :: (Monad m) => [[Pat]] -> [Pat] -> CompilerT a m Bool
isUseful [] _ = pure True -- zero rows (0x0 matrix)
isUseful px@(ps : _) qs =
  case (qs, length ps) of
    (_, 0) ->
      pure False -- one or more rows but no columns
    ([], _) ->
      error "Implementation error in pattern anomalies check"
    -- Pattern q_1 is a constructed pattern
    (Any : _, _)
      | all isRecRow px -> isUsefulRecord px [] Nothing
    (Rec fs rest : _, _) ->
      isUsefulRecord px fs rest
    (Con name rs : _, _) ->
      go name (length rs)
    (Or r1 r2 : _, _) ->
      isUseful px (r1 : qs) ||^ isUseful px (r2 : qs)
    (_ : qs1, _) -> do
      complete <- isComplete (fst <$> cs)
      if complete
        then cs & anyM (uncurry go)
        else isUseful (defaultMatrix px) qs1
 where
  cs = headCons px
  go name n = isUseful (specialized name n px) (head (specialized name n [qs]))

isRecRow :: [Pat] -> Bool
isRecRow (Rec{} : _) = True
isRecRow _ = False

hasOpenRecord :: [[Pat]] -> Bool
hasOpenRecord =
  any $
    \case
      (Rec _ (Just _) : _) -> True
      _ -> False

normalizeRec :: Bool -> [Name] -> [(Name, Pat)] -> Maybe Pat -> [Pat]
normalizeRec includeRest allFields fs rest =
  fields ++ restCol
 where
  m = Map.fromList fs
  fields = [Map.findWithDefault Any f m | f <- allFields]
  restCol
    | includeRest =
        case rest of
          Just p -> [p]
          Nothing -> [Lit LUnit] -- closed record marker
    | otherwise = []

isUsefulRecord :: (Monad m) => [[Pat]] -> [(Name, Pat)] -> Maybe Pat -> CompilerT a m Bool
isUsefulRecord px fs rest = do
  isUseful pMatrix qRow
 where
  includeRest = hasOpenRecord px || isJust rest

  allFields =
    Set.toList $
      Set.unions
        [ Set.fromList (map fst fs)
        , Set.unions
            [ Set.fromList (map fst fs')
            | (Rec fs' _ : _) <- px
            ]
        ]

  qRow =
    normalizeRec includeRest allFields fs rest

  pMatrix =
    [ normalizeRec includeRest allFields fs' rest' ++ ps
    | (Rec fs' rest' : ps) <- px
    ]

isComplete :: (Monad m) => [Name] -> CompilerT a m Bool
isComplete [] = pure False
isComplete names@(name : _) = do
  defined <- asks (mapEnvironment dataConstructorEntryNameSet . compilerDataConstructorEnvironment)
  case Environment.lookup name (defined `Environment.union` builtIn) of
    Nothing ->
      pure ("%Tuple" `Text.isPrefixOf` name)
    Just set_ ->
      pure (Set.fromList names == set_)
 where
  builtIn =
    anomaliesEnvironment
      [ ("%True", ["%True", "%False"])
      , ("%False", ["%True", "%False"])
      , ("%()", ["%()"])
      , ("::", ["::", "[]"])
      , ("[]", ["::", "[]"])
      , ("Zero", ["Zero", "Succ"])
      , ("Succ", ["Zero", "Succ"])
      , ("$Record", ["$Record"])
      ]

translatePattern :: Pattern a t -> Pat
translatePattern =
  \case
    PAnnotation _ _ p ->
      translatePattern p
    PAny{} ->
      Any
    PVariable{} ->
      Any
    PConstructor _ (Label _ name) ps ->
      Con name (translatePattern <$> ps)
    PLiteral _ p ->
      Lit p
    PInteger _ _ p ->
      Lit (LBignum p)
    PListCons _ _ p q ->
      Con "::" [translatePattern p, translatePattern q]
    PListLiteral _ _ ps ->
      foldr listCons (Con "[]" []) ps
    PTuple _ _ ps ->
      Con (tupleCons (length ps)) (translatePattern <$> NonEmpty.toList ps)
    POr _ _ p q ->
      Or (translatePattern p) (translatePattern q)
    PAs _ _ p ->
      translatePattern p
    PAtVariable{} ->
      Any
    PRecord _ _ fields rest ->
      Rec
        (sortOn fst (Map.toList (translatePattern <$> fields)))
        (translatePattern <$> rest)
    _ ->
      error "Not implemented"

{-# INLINE tupleCons #-}
tupleCons :: Int -> Name
tupleCons n = "%Tuple" <> showt n

listCons :: Pattern a t -> Pat -> Pat
listCons p q = Con "::" [translatePattern p, q]
