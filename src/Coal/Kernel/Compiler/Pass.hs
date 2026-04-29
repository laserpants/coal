module Coal.Kernel.Compiler.Pass (
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
  astSaturateConstructors,
) where

import Coal.Kernel.Compiler.AST (flattenLambdaNodes, flattenObject, simplifyLetNodes, sortMatchClauses)
import Coal.Kernel.Compiler.Pass.ClosureConversion (closeObjects)
import Coal.Kernel.Compiler.Pass.ExtraArgs (addImplicitArgs)
import Coal.Kernel.Compiler.Pass.LambdaLifting (liftLambdaNodes)
import Coal.Kernel.Compiler.Pass.LetLifting (liftLetNodes)
import Coal.Kernel.Compiler.Pass.Memoize (memoize)
import Coal.Kernel.Compiler.Pass.PartialConstructors (saturateConstructors)
import Coal.Kernel.Compiler.Pass.Suffix (suffixExpr)
import Coal.Kernel.Compiler.Pipeline (PipelineT, pipelineInsertArtifacts)
import Coal.Kernel.Compiler.Pipeline.State (PipelineState (..), overPipelineStateSupply)
import Coal.Kernel.LLVM (IRInterpreter, irInterpreterStateArtifacts, runInterpreter)
import Coal.Kernel.LLVM.IRError (prettyIRError)
import Coal.Kernel.Language
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (State, gets, modify, runState)
import Control.Monad.Writer (Writer, runWriter)
import Extras (traverse2)
import Extras.Control.Applicative (pure1, pure3)

transformSuffixMonad :: (MonadIO m) => State Int a -> PipelineT m a
transformSuffixMonad a = do
  (v, n) <- gets (runState a . pipelineSupply)
  modify (overPipelineStateSupply (const n))
  pure v

transformInterpreter :: (MonadIO m) => IRInterpreter a -> PipelineT m a
transformInterpreter p = do
  env <- gets pipelineInterpreterEnv
  let (result, s, _) = runInterpreter env p
  pipelineInsertArtifacts (irInterpreterStateArtifacts s)
  case result of
    Left err -> error ("IR generation error: " <> show (prettyIRError err))
    Right a -> pure a

type Pass m i o = i -> PipelineT m o

collectObjs :: (MonadIO m) => (Expr Type -> Writer [Binding Type (Expr Type)] (Expr Type)) -> Pass m ObjectList ObjectList
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
  , astSimplify1
  , astSaturateConstructors ::
    (MonadIO m) => Pass m ObjectList ObjectList
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
astSaturateConstructors =
  pure3 saturateConstructors
