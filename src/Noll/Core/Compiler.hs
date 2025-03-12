{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Compiler (compile) where

import Control.Arrow ((>>>))
import Control.Monad (void, (>=>))
import Control.Monad.State (State, gets, modify, runState)
import Control.Monad.Writer (MonadWriter, Writer, runWriter, tell)
import Data.Functor.Foldable (cata, embed, project)
import Data.List (nub, partition)
import Noll.Common.List1 (NonEmpty (..), fromList1)
import Noll.Core.Compiler.Ast
import Noll.Core.Compiler.Pass.ClosureConversion (closeDefs)
import Noll.Core.Compiler.Pass.ExtraArgs (addImplicitArgs)
import Noll.Core.Compiler.Pass.LambdaLifting (liftLambdas)
import Noll.Core.Compiler.Pass.LetLifting (transformLetLifting)
import Noll.Core.Compiler.Pass.Memoize (memoize)
import Noll.Core.Compiler.Pass.Suffix (transformSuffixExpr)
import Noll.Core.Compiler.Pipeline (Pipeline (..), extendInterpreterConstructorEnv, extendInterpreterValueEnv, pipelineInsertArtifacts, pipelineInsertCode)
import Noll.Core.Compiler.Pipeline.Kernel (Kernel (..), overKernelSupply)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IRInterpreter
import Noll.Core.LLVM.IRInterpreter.Environment
import Noll.Core.LLVM.IRInterpreter.Monad
import Noll.Core.LLVM.IRInterpreter.State
import Noll.Core.Language (Binding (..), Expr, Type, bindingLabel, isPrim)
import Noll.Core.Language.Object (Object (..), ObjectList)
import Noll.Core.Language.Type.Arrow (isFunction)
import Noll.Label (Label (..))
import Noll.Utils (Name, traverse2, (<$$>))
import Noll.Utils.Control.Applicative (pure1, pure3)
import Noll.Utils.Operators ((||.))

import qualified Noll.Common.Environment as Environment
import qualified Noll.Common.List1 as List1
import qualified Noll.Core.Language as Core

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
