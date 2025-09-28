{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.PatternAnomalies where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Language.Primitive (Primitive (..))
import Control.Monad.Extra (anyM)
import Control.Monad.Reader (MonadReader, ask)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Extra (Name, (<$$>))

type AnomaliesEnvironment = Environment (Set Name)

anomaliesEnvironment :: [(Name, [Name])] -> AnomaliesEnvironment
anomaliesEnvironment = Environment.fromList . (Set.fromList <$$>)

data Pat
  = Con Name [Pat]
  | Lit Primitive
  | Any
  deriving (Show, Eq, Ord, Read)

exhaustive :: (MonadReader AnomaliesEnvironment m) => [Pat] -> m Bool
exhaustive ps = not <$> isUseful ((: []) <$> ps) [Any]

specialized :: Name -> Int -> [[Pat]] -> [[Pat]]
specialized name a = concatMap go
 where
  go [] =
    error "Implementation error"
  go (p : ps) =
    case p of
      Con name' rs
        | name' == name -> [rs <> ps]
        | otherwise -> []
      _ ->
        [replicate a Any <> ps]

defaultMatrix :: [[Pat]] -> [[Pat]]
defaultMatrix = concatMap go
 where
  go (p : ps) =
    case p of
      Con{} ->
        []
      Lit{} ->
        []
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
      Lit p ->
        [(prim p, 0)]
      Con name rs ->
        [(name, length rs)]
      _ ->
        []

prim :: Primitive -> Name
prim =
  \case
    LUnit -> "%()"
    LBool True -> "%True"
    LBool False -> "%False"
    LInt32{} -> "%Int32"
    LInt64{} -> "%Int64"
    LBignum{} -> "%Bignum"
    LFloat{} -> "%Float"
    LDouble{} -> "%Double"
    LChar{} -> "%Char"
    LString{} -> "%String"

isUseful :: (MonadReader AnomaliesEnvironment m) => [[Pat]] -> [Pat] -> m Bool
isUseful [] _ = pure True -- zero rows (0x0 matrix)
isUseful px@(ps : _) qs =
  case (qs, length ps) of
    (_, 0) ->
      pure False -- one or more rows but no columns
    ([], _) ->
      error "Implementation error in pattern anomalies check"
    -- Pattern q_1 is a constructed pattern
    (Con name rs : _, _) ->
      go name (length rs)
    (_ : qs1, _) -> do
      complete <- isComplete (fst <$> cs)
      if complete
        then cs & anyM (uncurry go)
        else isUseful (defaultMatrix px) qs1
 where
  cs = headCons px
  go name n = isUseful (specialized name n px) (head (specialized name n [qs]))

isComplete :: (MonadReader AnomaliesEnvironment m) => [Name] -> m Bool
isComplete [] = pure False
isComplete names@(name : _) = do
  defined <- ask
  let constructors = defined `Environment.union` builtIn
      set_ = fromMaybe mempty (Environment.lookup name constructors)
  pure (Set.fromList names == set_)
 where
  builtIn =
    anomaliesEnvironment
      [ ("%True", ["%True", "%False"])
      , ("%False", ["%True", "%False"])
      , ("%()", ["%()"])
      , ("%Int32", [])
      , ("%Int64", [])
      , ("%Bignum", [])
      , ("%Float", [])
      , ("%Double", [])
      , ("%Char", [])
      , ("%String", [])
      ]
