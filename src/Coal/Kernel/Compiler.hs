{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.Compiler (compile, compileModules, KernelExpr) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Kernel.Builtin.Constructors (builtinConstructors)
import Coal.Kernel.Compiler.EntryPoint (entryPoint)
import Coal.Kernel.Compiler.Pass
import Coal.Kernel.Compiler.Pipeline
import Coal.Kernel.Compiler.Pipeline.State (PipelineState (..))
import Coal.Kernel.LLVM
import Coal.Kernel.Language
import Control.Monad (void, (>=>))
import Control.Monad.State (gets)
import Data.List (nub)
import qualified Data.Text as Text
import Extras (Name, forM, isConstructor, (<$$>), (||.))

type KernelExpr = Expr Type

corePass :: Pass ObjectList ObjectList
corePass =
  astSortMatchClauses
    >=> astSuffix
    >=> astFlatten
    >=> astLiftLetNodes
    >=> astMemoize
    >=> astLiftLambdaNodes
    >=> astSimplify1
    >=> astSimplify2
    >=> astCloseObjects
    >=> astAddExtraArgs

setLinkage :: [Name] -> IRConstruct a -> Pipeline (IRConstruct a)
setLinkage names =
  \case
    CDefine name t Nothing ts e
      | name `notElem` names ->
          pure (CDefine name t (Just LPrivate) ts e)
    c ->
      pure c

irCodeGen :: [Name] -> Pass ObjectList [IRConstruct [IRLine]]
irCodeGen names objs = do
  c1 <- transformInterpreter (traverse interpretObject objs)
  c2 <- gets pipelineArtifacts
  c3 <- transformInterpreter (traverse interpretArtifact (nub c2))
  c4 <- traverse (setLinkage names) (concat c1)
  pipelineInsertCode (support <> closureSupport <> concat c3 <> c4)
  gets pipelineCode

compile :: Environment Int -> Pass ObjectList ()
compile importedCtors input = do
  objs <- corePass input
  extendInterpreterValueEnv (objectEnvironment objs)
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

compileModules :: [Module Type Name KernelExpr] -> IO [(Name, [IRConstruct [IRLine]])]
compileModules modules =
  evalPipeline $ do
    mods <- forM modules $
      \Module{..} -> do
        pipelineReset
        names <- collectNames moduleImports
        ctors <- collectConstructors moduleImports

        ir <- compileModule ctors Module{moduleImports = names, ..}
        pipelineInsertNames (objectSignature <$> moduleObjects)

        IRInterpreterEnv{..} <- gets pipelineInterpreterEnv
        pipelineInsertIRTypes (irTypeOf <$$> Environment.toList irInterpreterValueEnv)

        pipelineInsertConstructors (objectConstructorInfo =<< moduleObjects)

        pure (moduleName, ir <> [entryPoint | moduleName == "Main"])
    cc <- transformInterpreter compileClosureCode
    pure (("closures", cc) : mods)
 where
  collectNames :: [Name] -> Pipeline [(Name, Type)]
  collectNames names = gets (Environment.lookupAll names . Environment.filterNames notConstructor . pipelineNames)

  notConstructor :: Name -> Bool
  notConstructor name =
    let parts = Text.splitOn "." name
     in not (isConstructor (last parts))

  collectConstructors :: [Name] -> Pipeline [(Name, Int)]
  collectConstructors names = gets (Environment.lookupAll names . pipelineConstructors)

  objectSignature :: Object Type KernelExpr -> (Name, Type)
  objectSignature obj = (objectName obj, typeOf obj)

  compileModule :: [(Name, Int)] -> Module Type (Name, Type) KernelExpr -> Pipeline [IRConstruct [IRLine]]
  compileModule ctors Module{..} = do
    names <- gets pipelineIRTypes
    let external = toExternal names <$> moduleImports
    compile (Environment.fromList ctors) (external <> moduleObjects)
    gets pipelineCode

  toExternal :: Environment IRType -> (Name, Type) -> Object Type KernelExpr
  toExternal env (name, t) =
    case Environment.lookup name env of
      Nothing ->
        error ("Name not in scope: '" <> Text.unpack name <> "'")
      Just it ->
        OExternal name it t
