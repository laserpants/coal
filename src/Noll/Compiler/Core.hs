{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Core where

import Control.Arrow ((>>>))
import Control.Monad.State (MonadState, State, StateT, evalState, evalStateT, gets, modify, runState, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata, embed, project)
import Data.Tuple.Extra (first)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Common.Supply (supplied)
import Noll.Core.Language (Clause (..), Expr, ExprF (..), Focus (..), Type, Typed (..), foldType)
import Noll.Core.Language.Replace (Sub, relabel)
import Noll.Core.Language.Typed (isFunction)
import Noll.Label (Label (..), labelName)
import Noll.Utils (Dictionary, Name, Over, applyM1, applyM2, foldrM, isConstructor, (<$$$>), (<$$>))
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

simplifyLets :: Expr t -> Expr t
simplifyLets e = relabel (Map.fromList sub) e1
 where
  subst =
    cata $
      \case
        ELet vs e1 -> do
          binds <- foldrM go [] =<< traverse sequence vs
          case binds of
            a : as ->
              Core.let_ (a :| as) <$> e1
            [] ->
              e1
        e ->
          embed <$> sequence e
  go (ll1, Fix (Core.EVar ll2)) ls = do
    tell [(labelName ll1, labelName ll2)]
    pure ls
  go l ls =
    pure (l : ls)
  (e1, sub) =
    runWriter (subst e)

-------------------------------------------------------------------------------

muteTypes :: Expr Type -> Expr ()
muteTypes =
  cata $
    \case
      EVar (Label _ name) ->
        Core.var (Label () name)
      ELet vs e ->
        Core.let_ (first muteLabelTypes <$> vs) e
      ELit p ->
        Core.lit p
      ELam lls e ->
        Core.lam (muteLabelTypes <$> lls) e
      EApp _ a es ->
        Core.app () a es
      EIf e1 e2 e3 ->
        Core.if_ e1 e2 e3
      EOp op ->
        Core.op op
      EMat _ e1 cs ->
        Core.match () e1 (muteClauseTypes <$> cs)
      EExt ll e1 e2 ->
        Core.ext (muteLabelTypes ll) e1 e2
      ENil ->
        Core.nil
      ESel (Focus name ll1 ll2) e1 e2 ->
        Core.sel (Focus name (muteLabelTypes ll1) (muteLabelTypes ll2)) e1 e2
      ECall ll es e ->
        Core.call (muteLabelTypes ll) es e

muteClauseTypes :: Clause Type (Expr ()) -> Clause () (Expr ())
muteClauseTypes (Clause lls e) = Clause (muteLabelTypes <$> lls) e

muteLabelTypes :: Label Type -> Label ()
muteLabelTypes (Label _ name) = Label () name

muteObjectTypes :: BlockObject Type (Expr Type) -> BlockObject () (Expr ())
muteObjectTypes =
  \case
    OFunction name lls e ->
      OFunction name (muteLabelTypes <$> lls) (muteTypes e)
    OConstant name e ->
      OConstant name (muteTypes e)
    OExternal name _ ->
      OExternal name ()

-------------------------------------------------------------------------------

data PipelineState = PipelineState
  { pipelineStateSupply :: Int
  }
  deriving (Show, Eq, Ord)

{-# INLINE overPipelineStateSupply #-}
overPipelineStateSupply :: Over PipelineState Int
overPipelineStateSupply f PipelineState{..} = PipelineState{pipelineStateSupply = f pipelineStateSupply, ..}

newtype Core a = Core {pipelineStack :: State PipelineState a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState PipelineState
    )

transSuffixMonad :: (MonadState PipelineState m) => State Int a -> m a
transSuffixMonad a = do
  (v, n) <- gets (runState a . pipelineStateSupply)
  modify (overPipelineStateSupply (const n))
  pure v

traverse2 :: (Applicative f, Traversable t1, Traversable t2) => (a -> f b) -> t2 (t1 a) -> f (t2 (t1 b))
traverse2 = traverse . traverse

suffixNamesC :: ObjectList -> Core ObjectList
suffixNamesC = transSuffixMonad . traverse2 transSuffixExpr

pure1 :: (Applicative f) => (a -> b) -> a -> f b
pure1 f = pure . f

pure2 :: (Applicative f1, Functor f2) => (a -> b) -> f2 a -> f1 (f2 b)
pure2 f = pure . (f <$>)

pure3 :: (Applicative f1, Functor f2, Functor f3) => (a -> b) -> f2 (f3 a) -> f1 (f2 (f3 b))
pure3 f = pure . (f <$$>)

-- simplifyLamsC :: ObjectList -> Core ObjectList
-- simplifyLamsC = pure3 simplifyLams
--
-- liftLambdasC :: ObjectList -> Core ObjectList
-- liftLambdasC = pure1 liftLambdas
--
-- simplifyLetsC :: ObjectList -> Core ObjectList
-- simplifyLetsC = pure3 simplifyLets

pipeline :: ObjectList -> Core ObjectList
pipeline ol = do
  a1 <- suffixNamesC ol
  a2 <- pure3 simplifyLams a1
  a3 <- pure1 liftLambdas a2
  a4 <- pure3 simplifyLets a3
  pure a4

runCore :: Core a -> (a, PipelineState)
runCore p = runState (pipelineStack p) (PipelineState 0)

-- xx1 objs = mapM_ print $ muteObjectTypes <$> liftLambdas (simplifyLams <$$> evalState (traverse (traverse transSuffixExpr) objs) 0)

xx2 objs = runCore (pipeline objs)

xx3 objs = muteObjectTypes <$> fst (xx2 objs)
