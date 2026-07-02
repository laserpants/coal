module Coal.LegacyKernel.Compiler.Pass (
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

import Coal.LegacyKernel.Compiler.AST (flattenLambdaNodes, flattenObject, simplifyLetNodes, sortMatchClauses)
import Coal.LegacyKernel.Compiler.Pass.ClosureConversion (closeObjects)
import Coal.LegacyKernel.Compiler.Pass.ExtraArgs (addImplicitArgs)
import Coal.LegacyKernel.Compiler.Pass.LambdaLifting (liftLambdaNodes)
import Coal.LegacyKernel.Compiler.Pass.LetLifting (liftLetNodes)
import Coal.LegacyKernel.Compiler.Pass.Memoize (memoize)
import Coal.LegacyKernel.Compiler.Pass.PartialConstructors (saturateConstructors)
import Coal.LegacyKernel.Compiler.Pass.Suffix (suffixExpr)
import Coal.LegacyKernel.Compiler.Pipeline (PipelineT, pipelineInsertArtifacts)
import Coal.LegacyKernel.Compiler.Pipeline.State (PipelineState (..), overPipelineStateSupply)
import Coal.LegacyKernel.LLVM (IRInterpreter, irInterpreterStateArtifacts, runInterpreter)
import Coal.LegacyKernel.LLVM.IRError (prettyIRError)
import Coal.LegacyKernel.Language
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
