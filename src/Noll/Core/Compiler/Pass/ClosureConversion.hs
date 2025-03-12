{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Core.Compiler.Pass.ClosureConversion (closeDefs) where

import Control.Arrow ((>>>))
import Control.Monad.RWS (RWS, evalRWS, tell)
import Data.Functor.Foldable (cata, embed)
import Noll.AST.FreeVars (FreeVars (..), exceptNames)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Core.Compiler.Ast (flattenEApp)
import Noll.Core.Language (Expr, Type)
import Noll.Core.Language.Object (Object (..), ObjectList, objectName)
import Noll.Label (Label (..), labelName)
import Noll.Utils (Name, Set, isConstructor, (<$$>))

import qualified Data.Set as Set
import qualified Noll.Core.Language as Core

evalWS0 :: RWS () w Int a -> (a, w)
evalWS0 v = evalRWS v () 0

{-# INLINE notConstructor #-}
notConstructor :: Label t -> Bool
notConstructor = not . isConstructor . labelName

freeSet :: (Foldable f, FreeVars e t) => f Name -> e -> Set (Label t)
freeSet names obj = Set.filter notConstructor (freeIn obj `exceptNames` names)

closeDefs :: ObjectList -> ObjectList
closeDefs objs = uncurry app (evalWS0 (traverse closed objs))
 where
  app objs1 args
    | null (snd =<< args) =
        objs1
    | otherwise =
        closeDefs (foldr (uncurry (fmap . fmap <$$> applyArgs)) objs1 args)
  names =
    Set.fromList (objectName <$> objs)
  closed obj = do
    let extra = Set.toList (freeSet names obj)
    case obj of
      OFunction name lls expr -> do
        tell [(name, extra)]
        pure (OFunction name (extra <> lls) expr)
      OConstant name expr -> do
        tell [(name, extra)]
        pure (OFunction name extra expr)
      OExternal name t ->
        pure (OExternal name t)

applyArgs :: Name -> [Label Type] -> Expr Type -> Expr Type
applyArgs _ [] = id
applyArgs name (a : as) =
  flattenEApp
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
