{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Core where

import Control.Arrow ((>>>))
import Control.Monad.State (MonadState, StateT, evalStateT, modify, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata, embed, project)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Common.Supply (supplied)
import Noll.Core.Language (Clause (..), Expr, Focus (..), Type, Typed (..), foldType)
import Noll.Core.Language.Replace (Sub, relabel)
import Noll.Core.Language.Typed (isFunction)
import Noll.Label (Label (..), labelName)
import Noll.Utils (Dictionary, Name, applyM1, applyM2, isConstructor)
import TextShow

import qualified Data.Map.Strict as Map
import qualified Noll.Common.List1 as List1
import qualified Noll.Core.Language as Core

data BlockObject t e
  = OFunction Name [Label t] e
  | OConstant Name e
  | OExternal Name t
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type ObjectList = [BlockObject Type (Expr Type)]

-------------------------------------------------------------------------------

runLifting :: StateT Int (Writer ObjectList) a -> (a, ObjectList)
runLifting e = runWriter (evalStateT e 1)

functionType :: (Functor f, Foldable f, Typed t, Typed u) => t -> f u -> Type
functionType a as = foldType (typeOf a) (typeOf <$> as)

liftLambdas :: ObjectList -> ObjectList
liftLambdas objs = objs1 <> objs2
 where
  (objs1, objs2) =
    runLifting (traverse (traverse go) objs)
  go =
    cata $
      \case
        Core.ELam vs e -> do
          n <- supplied id
          let name = "$anon." <> showt n
          moveUp name vs =<< e
        e ->
          embed <$> sequence e

moveUp :: (MonadWriter ObjectList m) => Name -> List1 (Label Type) -> Expr Type -> m (Expr Type)
moveUp name vs f = do
  tell [OFunction name (fromList1 vs) f]
  pure (Core.var (Label (functionType f vs) name))

-------------------------------------------------------------------------------

letLiftFromExpression :: Name -> Expr Type -> ObjectList
letLiftFromExpression name expr = toBlockObject name e : (uncurry bob <$> objs)
 where
  bob = toBlockObject . labelName
  (e, objs) = runWriter (transLetLifting expr)

toBlockObject :: Name -> Expr Type -> BlockObject Type (Expr Type)
toBlockObject name =
  project
    >>> \case
      Core.ELam vs e ->
        OFunction name (fromList1 vs) e
      e ->
        OConstant name (embed e)

transLetLifting :: (MonadWriter [(Label Type, Expr Type)] m) => Expr Type -> m (Expr Type)
transLetLifting =
  cata $
    \case
      Core.ELet vs e -> do
        as <- traverse sequence vs
        let (fs, es) = List1.partition (isFunction . fst) as
        tell fs
        case es of
          w : ws ->
            Core.let_ (w :| ws) <$> e
          [] ->
            e
      e ->
        embed <$> sequence e

-------------------------------------------------------------------------------

transSuffixExpr :: (MonadState Int m) => Expr t -> m (Expr t)
transSuffixExpr =
  cata $
    \case
      Core.ELet vs e -> do
        let (lls, es) = List1.unzip vs
        (lls1, a1, a2) <- applyM2 (addSuffix2 lls) (sequence es) e
        pure (Core.let_ (List1.zip lls1 a1) a2)
      Core.ELam lls e -> do
        (lls1, a1) <- applyM1 (addSuffix lls) e
        pure (Core.lam lls1 a1)
      Core.ESel (Focus name ll2 ll3) e1 e2 -> do
        a1 <- e1
        (lls1, a2) <- applyM1 (addSuffix (ll2 :| [ll3])) e2
        case lls1 of
          (lls4 :| lls5 : _) ->
            pure (Core.sel (Focus name lls4 lls5) a1 a2)
          _ ->
            error "Implementation error"
      Core.EMat t e cs ->
        Core.match t
          <$> e
          <*> (traverse transSuffixClause =<< traverse sequence cs)
      e ->
        embed <$> sequence e

transSuffixClause :: (MonadState Int m) => Clause t (Expr t) -> m (Clause t (Expr t))
transSuffixClause =
  \case
    Clause lls e -> do
      (lls1, a) <- addSuffix lls e
      pure (Clause lls1 a)

addSuffix :: (MonadState Int m, Sub s) => List1 (Label t) -> s -> m (List1 (Label t), s)
addSuffix lls e = do
  (lls1, sub) <- mapping lls
  pure (lls1, relabel sub e)

addSuffix2 :: (MonadState Int m, Sub s1, Sub s2) => List1 (Label t) -> s1 -> s2 -> m (List1 (Label t), s1, s2)
addSuffix2 lls e1 e2 = do
  (lls1, sub) <- mapping lls
  pure (lls1, relabel sub e1, relabel sub e2)

mapping :: (MonadState Int m) => List1 (Label t) -> m (List1 (Label t), Dictionary Name)
mapping lls = runStateT (traverse go lls) mempty
 where
  go ll@(Label t name)
    | isConstructor name =
        pure ll
    | otherwise = do
        n <- lift (supplied id)
        let name1 = name <> ".[" <> showt n <> "]"
        modify (Map.insert name name1)
        pure (Label t name1)

-------------------------------------------------------------------------------

simplifyLams :: Expr Type -> Expr Type
simplifyLams =
  cata $
    \case
      Core.ELam vs1 (Fix (Core.ELam vs2 e1)) ->
        Core.lam (vs1 <> vs2) e1
      e ->
        embed e

-------------------------------------------------------------------------------

data CorePipeline e = CorePipeline e
