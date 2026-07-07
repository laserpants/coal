{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Kernel.LLVM.TestUtils
Description: Common utilities for LLVM code generation tests

This module provides shared utilities for testing the Coal LLVM code generator,
including built-in data constructors and the core evaluation function.
-}
module Coal.Kernel.LLVM.TestUtils (
  -- * Built-in Data Constructors
  builtinObjects,
  injectBuiltins,

  -- * Evaluation
  evaluateFoo,
) where

import Coal.Kernel.LLVM.Codegen (irModule)
import Coal.Kernel.LLVM.Monad (IRCodegenEnv, runIRCodegen)
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type (..))
import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (runStateT)
import LLVM.IR (IRBuilderEnv, IRModule, runIRBuilder)

{- | Coal builtin data constructors for testing.

These provide basic types needed for LLVM tests: Lists, Records, and Tuples.
Currently hardcoded here; in production these would come from a prelude module.

Injected into test modules automatically so tests don't have to declare them
explicitly. The system automatically validates that imported constructors exist
across all loaded modules, and that there are no linker errors.

With the grouped DData representation, constructors are sorted lexicographically
within each type group:
  * List type: @$Cons@ (index 0), @$Nil@ (index 1)
  * record type: @$Record@ (index 0)
  * tuple2 type: @$Tuple2@ (index 0)

Because they carry a @$@ prefix, 'irDataConstructor' emits them with
'LInternal' linkage, so each translation unit gets its own private copy and
there are no duplicate-symbol linker errors.
-}
builtinObjects :: [Object Type]
builtinObjects =
  [ DData
      "List"
      [ ("$Cons", TCon "/" [TOpq, TCon "/" [TCon "List" [TOpq], TCon "List" [TOpq]]])
      , ("$Nil", TCon "List" [TOpq])
      ]
  , DData
      "record"
      [ ("$Record", TCon "/" [TOpq, TCon "record" [TOpq]])
      ]
  , DData
      "tuple2"
      [ ("$Tuple2", TCon "/" [TOpq, TCon "/" [TOpq, TCon "tuple2" [TOpq, TOpq]]])
      ]
  ]

-- | Prepend 'builtinObjects' to a module's object list.
injectBuiltins :: Module Type -> Module Type
injectBuiltins m = m{moduleObjects = builtinObjects <> moduleObjects m}

{- | Evaluate a Coal module to LLVM IR.

This is the core test utility that runs the full IR codegen pipeline on a module,
returning either an error message or the generated IR module.
-}
evaluateFoo :: IRCodegenEnv -> IRBuilderEnv -> [Module Type] -> Module Type -> Either String IRModule
evaluateFoo codeGenEnv builderEnv allModules module_ =
  case runIdentity $ runExceptT $ runStateT (runIRBuilder (runIRCodegen codeGenEnv (irModule allModules module_))) builderEnv of
    Left builderErr ->
      Left (show builderErr)
    Right (Left codeGenErr, _) ->
      Left (show codeGenErr)
    Right (Right r, _) ->
      Right r
