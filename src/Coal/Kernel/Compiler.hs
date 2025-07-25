{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.Compiler (compile, compileModules) where

import Coal.Common.Environment (Environment (..))
import Coal.Kernel.Compiler.Pass
import Coal.Kernel.Compiler.Pipeline
import Coal.Kernel.Compiler.Pipeline.Kernel (Kernel (..))
import Coal.Kernel.LLVM
import Coal.Kernel.Language
import Control.Monad (void, (>=>))
import Control.Monad.State (gets)
import Data.List (nub)
import Extra (Name, forM, isConstructor, (<$$>), (||.))

import qualified Coal.Common.Environment as Environment
import qualified Data.Text as Text

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
  c2 <- gets kernelArtifacts
  c3 <- transformInterpreter (traverse interpretArtifact (nub c2))
  c4 <- traverse (setLinkage names) (concat c1)
  pipelineInsertCode (support <> closureSupport <> concat c3 <> c4)
  gets kernelCode

builtInCtors :: Environment Int
builtInCtors = Environment.fromList [("$Cons", 0), ("$Nil", 1), ("$Succ", 0), ("$Zero", 1)]

compile :: Environment Int -> Pass ObjectList ()
compile importedCtors input = do
  objs <- corePass input
  extendInterpreterValueEnv (objectEnvironment objs)
  extendInterpreterConstructorEnv (builtInCtors <> importedCtors <> objectConstructors objs)
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

compileModules :: [Module Type Name (Expr Type)] -> IO [(Name, [IRConstruct [IRLine]])]
compileModules modules =
  evalPipeline $ do
    mods <- forM modules $
      \Module{..} -> do
        pipelineReset
        names <- collectNames moduleImports
        ctors <- collectConstructors moduleImports

        ir <- compileModule ctors Module{moduleImports = names, ..}
        pipelineInsertNames (objectSignature <$> moduleObjects)

        IRInterpreterEnv{..} <- gets kernelInterpreterEnv
        pipelineInsertIRTypes (irTypeOf <$$> Environment.toList irInterpreterValueEnv)

        pipelineInsertConstructors (concatMap objectConstructorInfo moduleObjects)

        pure (moduleName, ir <> [entryPoint | moduleName == "Main"])
    cc <- transformInterpreter compileClosureCode
    pure (("closures", cc) : mods)
 where
  collectNames :: [Name] -> Pipeline [(Name, Type)]
  collectNames names = gets (Environment.lookupAll names . Environment.filterNames notConstructor . kernelNames)

  notConstructor :: Name -> Bool
  notConstructor name =
    let parts = Text.splitOn "." name
     in not (isConstructor (last parts))

  collectConstructors :: [Name] -> Pipeline [(Name, Int)]
  collectConstructors names = gets (Environment.lookupAll names . kernelConstructors)

  objectSignature :: Object Type (Expr Type) -> (Name, Type)
  objectSignature obj = (objectName obj, typeOf obj)

  compileModule :: [(Name, Int)] -> Module Type (Name, Type) (Expr Type) -> Pipeline [IRConstruct [IRLine]]
  compileModule ctors Module{..} = do
    names <- gets kernelIRTypes
    let external = toExternal names <$> moduleImports
    compile (Environment.fromList ctors) (external <> moduleObjects)
    gets kernelCode

  toExternal :: Environment IRType -> (Name, Type) -> Object Type (Expr Type)
  toExternal env (name, t) =
    case Environment.lookup name env of
      Nothing ->
        error ("Name not in scope: '" <> Text.unpack name <> "'")
      Just it ->
        OExternal name it t

  entryPoint :: IRConstruct [IRLine]
  entryPoint =
    let instrs =
          [ LInstruction ["call void @gc_init()"]
          , LInstruction ["call void @\"Main.main\"(i8* null)"]
          , LInstruction ["ret i32 0"]
          ]
     in CDefine "main" i32 Nothing [] instrs
