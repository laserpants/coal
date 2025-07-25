module Noll.Kernel.Compiler.Pass (
  Pass,
  transformInterpreter,
  transformSuffixMonad,
  astSortMatchClauses,
  astSuffix,
  astFlatten,
  astLiftLetNodes,
  astMemoize,
  astLiftLambdaNodes,
  astSimplify2,
  astCloseObjects,
  astAddExtraArgs,
  astSimplify1,
) where

import Control.Monad.State (State, gets, modify, runState)
import Control.Monad.Writer (Writer, runWriter)
import Extra (traverse2)
import Extra.Control.Applicative (pure1, pure3)
import Noll.Kernel.Compiler.Ast (flattenLambdaNodes, flattenObject, simplifyLetNodes, sortMatchClauses)
import Noll.Kernel.Compiler.Pass.ClosureConversion (closeObjects)
import Noll.Kernel.Compiler.Pass.ExtraArgs (addImplicitArgs)
import Noll.Kernel.Compiler.Pass.LambdaLifting (liftLambdaNodes)
import Noll.Kernel.Compiler.Pass.LetLifting (liftLetNodes)
import Noll.Kernel.Compiler.Pass.Memoize (memoize)
import Noll.Kernel.Compiler.Pass.Suffix (suffixExpr)
import Noll.Kernel.Compiler.Pipeline
import Noll.Kernel.Compiler.Pipeline.Kernel (Kernel (..), overKernelSupply)
import Noll.Kernel.LLVM (IRInterpreter, irInterpreterStateArtifacts, runInterpreter)
import Noll.Kernel.Language

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
collectObjs f as = pure (ls <> fmap fromBinding rs)
 where
  (ls, rs) = runWriter (traverse2 f as)

astSortMatchClauses
  , astSuffix
  , astFlatten
  , astLiftLetNodes
  , astMemoize
  , astLiftLambdaNodes
  , astSimplify2
  , astCloseObjects
  , astAddExtraArgs
  , astSimplify1 ::
    Pass ObjectList ObjectList
astSortMatchClauses =
  pure3 sortMatchClauses
astSuffix =
  transformSuffixMonad . traverse2 suffixExpr
astFlatten =
  pure3 flattenLambdaNodes
astLiftLetNodes =
  collectObjs liftLetNodes
astMemoize =
  collectObjs memoize
astLiftLambdaNodes =
  pure1 liftLambdaNodes
astSimplify2 =
  pure3 simplifyLetNodes
astCloseObjects =
  pure1 closeObjects
astAddExtraArgs =
  pure1 (fmap addImplicitArgs)
astSimplify1 =
  pure1 (fmap flattenObject)
