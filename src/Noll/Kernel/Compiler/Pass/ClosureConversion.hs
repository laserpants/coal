{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Kernel.Compiler.Pass.ClosureConversion (closeObjects) where

import Control.Arrow ((>>>))
import Control.Monad.RWS (RWS, evalRWS, tell)
import Data.Fix (Fix (..))
import Data.Function (on)
import Data.Functor.Foldable (cata, embed)
import Data.List (nubBy)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Common.FreeVars (freeSet)
import Noll.Common.Label (Label (..))
import Noll.Kernel.Compiler.Ast (flattenAppNodes)
import Noll.Kernel.Language (Expr, Type)
import Noll.Kernel.Language.Object (Object (..), ObjectList, objectName)
import Extra (Name, (<$$>))

import qualified Data.Set as Set
import qualified Noll.Kernel.Language as Core

evalWS0 :: RWS () w Int a -> (a, w)
evalWS0 v = evalRWS v () 0

closeObjects :: ObjectList -> ObjectList
closeObjects objs = uncurry app (evalWS0 (traverse closed objs))
 where
  app objs1 args
    | null (snd =<< args) =
        objs1
    | otherwise =
        closeObjects (foldr (uncurry (fmap . fmap <$$> applyArgs)) objs1 args)
  names =
    Set.fromList (objectName <$> objs)
  closed obj = do
    let extra = nubBy ((==) `on` labelName) (Set.toList (freeSet names obj))
    case obj of
      OFunction name lls expr -> do
        tell [(name, extra)]
        pure (OFunction name (extra <> lls) expr)
      OConstant name expr@(Fix Core.ELit{}) | null extra -> do
        pure (OConstant name expr)
      OConstant name expr -> do
        tell [(name, extra)]
        pure (OFunction name extra expr)
      OExternal name it t ->
        pure (OExternal name it t)
      OData name i t ->
        pure (OData name i t)

applyArgs :: Name -> [Label Type] -> Expr Type -> Expr Type
applyArgs _ [] = id
applyArgs name (a : as) =
  flattenAppNodes
    >>> cata
      ( \case
          Core.EVar (Label t n)
            | name == n -> do
                let expr = Core.var (Label (Core.foldType t (Core.typeOf <$> (a : as))) n)
                Core.app t expr (Core.var <$> a :| as)
            | otherwise ->
                Core.var (Label t n)
          e ->
            embed e
      )
