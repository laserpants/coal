{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Compiler (compile) where

import Control.Arrow ((>>>))
import Control.Monad (void, (>=>))
import Control.Monad.State (State, gets, modify, runState)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Functor.Foldable (cata, embed, project)
import Data.List (nub, partition)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Core.Compiler.Ast
import Noll.Core.Compiler.Pass.ClosureConversion (closeDefs)
import Noll.Core.Compiler.Pass.LambdaLifting (liftLambdas)
import Noll.Core.Compiler.Pass.Suffix (transformSuffixExpr)
import Noll.Core.Compiler.Pipeline (Pipeline (..), extendInterpreterConstructorEnv, extendInterpreterValueEnv, pipelineInsertArtifacts, pipelineInsertCode)
import Noll.Core.Compiler.Pipeline.Kernel (Kernel (..), overKernelSupply)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IRInterpreter
import Noll.Core.LLVM.IRInterpreter.Environment
import Noll.Core.LLVM.IRInterpreter.Monad
import Noll.Core.LLVM.IRInterpreter.State
import Noll.Core.Language (Binding (..), Expr, Type, Typed (..), bindingLabel, isPrim)
import Noll.Core.Language.Object (Object (..), ObjectList)
import Noll.Core.Language.Type.Arrow (isFunction)
import Noll.Label (Label (..))
import Noll.Utils (Name, traverse2, (<$$>))
import Noll.Utils.Control.Applicative (pure1, pure3)
import Noll.Utils.Operators ((||.))
import TextShow (showt)

import qualified Noll.Common.Environment as Environment
import qualified Noll.Common.List1 as List1
import qualified Noll.Core.Language as Core

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
