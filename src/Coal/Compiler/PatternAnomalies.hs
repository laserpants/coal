{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.PatternAnomalies where

import Coal.Common.Environment (Environment (..))
import Coal.Language.Primitive (Primitive (..))
import Control.Monad.Reader (MonadReader, ask)
import Extra (Name)

type AnomaliesEnvironment = Environment [Name]

data Pat
  = Con Name [Pat]
  | Lit Primitive
  | Any
  deriving (Show, Eq, Ord, Read)

specialized :: Int -> Name -> [[Pat]] -> [[Pat]]
specialized a name = concatMap go
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
    (Con name rs : _, _) -> do
      undefined
      undefined
    (_ : qs1, _) -> do
      let cs = headCons px
      undefined

isComplete :: (MonadReader AnomaliesEnvironment m) => [Name] -> m Bool
isComplete [] = pure False
isComplete names = do
  defined <- ask
  undefined

--        (Fix (ConP name rs):_, _) ->
--            let special = specialized name (length rs)
--             in useful (special px) (head (special [qs]))
--
--        (_:qs1, _) -> do
--            cs <- headCons px
--            isComplete <- complete (fst <$> cs)
--            if isComplete
--                then cs & anyM (\con ->
--                    let special = uncurry specialized con
--                     in useful (special px) (head (special [qs])))
--                else useful (defaultMatrix px) qs1
