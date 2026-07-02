{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.LegacyKernel.Compiler (
  compile,
  compileClosureCode,
  KernelExpr,
  KernelModule,
) where

import Coal.Common.Environment (Environment (..))
import Coal.LegacyKernel.Builtin.Constructors (builtinConstructors)
import Coal.LegacyKernel.Compiler.Pass
import Coal.LegacyKernel.Compiler.Pipeline
import Coal.LegacyKernel.Compiler.Pipeline.State (PipelineState (..))
import Coal.LegacyKernel.LLVM
import Coal.LegacyKernel.Language
import Control.Monad (void, (>=>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Data.List (nub, sort)
import Extras (Name, (||.))

type KernelExpr = Expr Type

type KernelModule = Module Type Name (Expr Type)

corePass :: (MonadIO m) => Pass m ObjectList ObjectList
corePass =
  astSortMatchClauses
    >=> astSuffix
    >=> astFlatten
    >=> astSaturateConstructors
    --    >=> astMemoize
    --    >=> astLiftLetNodes
    >=> astLiftLambdaNodes
    >=> astSimplify1
    >=> astSimplify2
    >=> astCloseObjects
    >=> astAddExtraArgs

setLinkage :: (MonadIO m) => [Name] -> IRConstruct a -> PipelineT m (IRConstruct a)
setLinkage names =
  \case
    CDefine name t Nothing ts e
      | name `notElem` names ->
          pure (CDefine name t (Just LPrivate) ts e)
    c ->
      pure c

irCodeGen :: (MonadIO m) => [Name] -> Pass m ObjectList [IRConstruct [IRLine]]
irCodeGen names objs = do
  c1 <- transformInterpreter (traverse interpretObject objs)
  c2 <- gets pipelineArtifacts
  c3 <- transformInterpreter (traverse interpretArtifact (nub (sort c2)))
  c4 <- traverse (setLinkage names) (concat c1)
  pipelineInsertCode (support <> closureSupport <> concat c3 <> c4)
  gets pipelineCode

compile :: (MonadIO m) => Environment Int -> Pass m ObjectList ()
compile importedCtors input = do
  objs <- corePass input
  irTypes <- gets pipelineIRTypes
  extendInterpreterValueEnv (objectEnvironment irTypes objs)
  extendInterpreterIRTypes irTypes
  extendInterpreterConstructorEnv (builtinConstructors <> importedCtors <> objectConstructors objs)
  void (irCodeGen names objs)
 where
  names =
    objectName <$> filter (objectIsFunction ||. objectIsConstant) input

compileClosureCode :: IRInterpreter [IRConstruct [IRLine]]
compileClosureCode = do
  closureDefinitions <-
    sequence
      [ irExtend
      , irFinalize
      , irApply
      , pure irCallTable
      , irCallN
      ]
  callDefinitions <- irCalls
  pure (support <> closureDefinitions <> callDefinitions)
