{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Compiler where

import Control.Arrow ((>>>))
import Control.Monad (void, (>=>))
import Control.Monad.RWS (RWS, ask, evalRWS, local)
import Control.Monad.State (MonadState, State, evalState, gets, modify, runState, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Fix (Fix (..))
import Data.Functor ((<&>))
import Data.Functor.Foldable (cata, embed, project)
import Data.List (nub, partition)
import Data.Set (Set)
import Noll.AST.FreeVars (FreeVars (..), exceptNames)
import Noll.Common.Environment (Environment)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Common.Supply (supplied)
import Noll.Core.Compiler.Ast
import Noll.Core.Compiler.Pipeline (Pipeline (..), extendInterpreterConstructorEnv, extendInterpreterValueEnv, pipelineInsertArtifacts, pipelineInsertCode)
import Noll.Core.Compiler.Pipeline.Kernel (Kernel (..), initialKernel, overKernelArtifacts, overKernelCode, overKernelInterpreterConstructorEnv, overKernelInterpreterValueEnv, overKernelSupply)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IRInterpreter
import Noll.Core.LLVM.IRInterpreter.Artifact
import Noll.Core.LLVM.IRInterpreter.Environment
import Noll.Core.LLVM.IRInterpreter.Monad
import Noll.Core.LLVM.IRInterpreter.State
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Core.Language (
  Binding (..),
  Clause (..),
  Expr,
  ExprF (..),
  Focus (..),
  Type,
  Typed (..),
  bindingLabel,
  foldType,
  functionTypeOf,
  isPrim,
  overBindingLabel,
  unzipBindings,
 )
import Noll.Core.Language.Expr.Replace (Sub, relabel)
import Noll.Core.Language.Object (Object (..), ObjectList, objectName)
import Noll.Core.Language.Type.Arrow (isFunction)
import Noll.Label (Label (..), labelName)
import Noll.Utils (
  Dictionary,
  Name,
  Over,
  applyM1,
  applyM2,
  foldrM,
  forM,
  isConstructor,
  traverse2,
  (<$$>),
 )
import Noll.Utils.Control.Applicative (pure1, pure3)
import Noll.Utils.Operators ((||.))
import TextShow

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Noll.Common.Environment as Environment
import qualified Noll.Common.List1 as List1
import qualified Noll.Core.Language as Core

-------------------------------------------------------------------------------

runLifting :: RWS Name ObjectList Int a -> (a, ObjectList)
runLifting e = evalRWS e "" 1

liftLambdas :: ObjectList -> ObjectList
liftLambdas objs = objs1 <> objs2
 where
  (objs1, objs2) =
    runLifting (traverse (traverse go) objs)
  go =
    cata $
      \case
        Core.ELet vs e -> do
          ws <- forM vs $ \(Binding ll@(Label _ name) e1) -> do
            f <- local (const name) e1
            pure (Binding ll f)
          f <- local mempty e
          pure (Core.let_ ws f)
        Core.ELam vs e -> do
          n <- supplied id
          name <- ask
          f <- local mempty e
          moveUp (if Text.null name then "$fn." <> showt n else name) vs f
        e ->
          local mempty (embed <$> sequence e)

moveUp :: (MonadWriter ObjectList m) => Name -> List1 (Label Type) -> Expr Type -> m (Expr Type)
moveUp name vs f = do
  tell [OFunction name (fromList1 vs) f]
  pure (Core.var (Label (functionTypeOf f vs) name))

-------------------------------------------------------------------------------

toObject :: Binding Type (Expr Type) -> Object Type (Expr Type)
toObject (Binding (Label _ name) e1) = go e1
 where
  go =
    project
      >>> \case
        Core.ELam vs e ->
          OFunction name (fromList1 vs) e
        e ->
          OConstant name (embed e)

memoize :: (MonadWriter [Binding Type (Expr Type)] m) => Expr Type -> m (Expr Type)
memoize =
  project
    >>> \case
      Core.ELet vs e -> do
        let (ps, qs) = partition (not . (isFunction . bindingLabel ||. isPrim . bindingExpr)) (fromList1 vs)
        tell (Core.mem <$$> ps)
        case qs of
          u : us ->
            pure (Core.let_ (u :| us) e)
          [] ->
            pure e
      e ->
        pure (embed e)

transformLetLifting :: (MonadWriter [Binding Type (Expr Type)] m) => Expr Type -> m (Expr Type)
transformLetLifting =
  cata $
    \case
      Core.ELet vs e -> do
        as <- traverse sequence vs
        let (fs, es) = List1.partition (isFunction . bindingLabel) as
        tell fs
        case es of
          w : ws ->
            Core.let_ (w :| ws) <$> e
          [] ->
            e
      e ->
        embed <$> sequence e

-------------------------------------------------------------------------------

transformSuffixExpr :: (MonadState Int m) => Expr t -> m (Expr t)
transformSuffixExpr =
  cata $
    \case
      Core.ELet vs e -> do
        let (lls, es) = unzipBindings vs
        (lls1, a1, a2) <- applyM2 (addSuffix2 lls) (sequence es) e
        pure (Core.let_ (List1.zipWith Binding lls1 a1) a2)
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
          <*> (traverse transformSuffixClause =<< traverse sequence cs)
      e ->
        embed <$> sequence e

transformSuffixClause :: (MonadState Int m) => Clause t (Expr t) -> m (Clause t (Expr t))
transformSuffixClause =
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

-------------------------------------------------------------------------------

addImplicitArgs :: Object Type (Expr Type) -> Object Type (Expr Type)
addImplicitArgs =
  \case
    f@(OFunction name lls1 expr)
      | isExprFun ->
          OFunction
            name
            (lls1 <> lls2)
            (flattenEApp (Core.app (List1.last ts) expr (exprs lls2)))
      | otherwise ->
          f
     where
      isExprFun =
        length ts > 1
      ts =
        Core.unfoldType (typeOf expr)
      lls2 =
        labels (List1.init ts)
    o ->
      o

exprs :: [Label t] -> List1 (Expr t)
exprs (ll : lls) = Core.var <$> ll :| lls
exprs _ = error "Implementation error"

labels :: [a] -> [Label a]
labels ts = zipWith Label ts ["$extra." <> showt i | i <- [0 :: Int ..]]

-------------------------------------------------------------------------------

transformSuffixMonad :: State Int a -> Pipeline a
transformSuffixMonad a = do
  (v, n) <- gets (runState a . kernelSupply)
  modify (overKernelSupply (const n))
  pure v

transformInterpreter :: IRInterpreter a -> Pipeline a
transformInterpreter p = do
  env <- gets kernelInterpreterEnv
  let (a, s, _) = runInterpreter env p
  pipelineInsertArtifacts (irInterpreterStateArtifacts s)
  pure a

type Pass i o = i -> Pipeline o

collectObjs :: (Expr Type -> Writer [Binding Type (Expr Type)] (Expr Type)) -> Pass ObjectList ObjectList
collectObjs f as = pure (ls <> fmap toObject rs)
 where
  (ls, rs) = runWriter (traverse2 f as)

coreSortMatchClauses
  , coreSuffix
  , coreFlatten
  , coreLetTranslation
  , coreMemoize
  , coreLambdaLifting
  , coreSimplify
  , coreCloseDefs
  , coreExtraArgs ::
    Pass ObjectList ObjectList
coreSortMatchClauses = pure3 sortMatchClauses
coreSuffix = transformSuffixMonad . traverse2 transformSuffixExpr
coreFlatten = pure3 flattenELam
coreLetTranslation = collectObjs transformLetLifting
coreMemoize = collectObjs memoize
coreLambdaLifting = pure1 liftLambdas
coreSimplify = pure3 simplifyELet
coreCloseDefs = pure1 closeDefs
coreExtraArgs = pure1 (fmap addImplicitArgs)

corePass :: Pass ObjectList ObjectList
corePass =
  coreSortMatchClauses
    >=> coreSuffix
    >=> coreFlatten
    >=> coreLetTranslation
    >=> coreMemoize
    >=> coreLambdaLifting
    >=> coreSimplify
    >=> coreCloseDefs
    >=> coreExtraArgs

irCodeGenPass :: Pass ObjectList [IRConstruct [IRLine]]
irCodeGenPass objs = do
  code <- transformInterpreter (traverse interpretObject objs)
  arts <- gets kernelArtifacts
  defs <- transformInterpreter (traverse interpretArtifact (nub arts))
  pipelineInsertCode (concat defs <> code)
  gets kernelCode

compile :: [(Name, Int)] -> Pass ObjectList ()
compile ctrs ol = do
  objs <- corePass ol
  extendInterpreterValueEnv (objectEnvironment objs)
  extendInterpreterConstructorEnv (Environment.fromList ctrs)
  void (irCodeGenPass objs)
