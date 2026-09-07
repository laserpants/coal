{-# LANGUAGE LambdaCase #-}

{- |
Module: Coal.Compiler.Pass.PhasePreflight.CheckTupleArity

Detect tuple expressions and patterns whose arity exceeds the maximum that
the kernel code generator supports.

The kernel LLVM codegen only declares tuple constructor functions for arities
2 through 'Coal.Language.Type.Operations.maxTupleArity' (see
'Coal.Compiler.Pass.PhaseLowering.KernelCodegen'). Larger tuples would
translate to references of undefined @make_%@$Tuple<N>@ functions and crash
@llvm-as@. This pass reports a statically checked, source-located diagnostic
instead.
-}
module Coal.Compiler.Pass.PhasePreflight.CheckTupleArity (
  passCheckTupleArity,
) where

import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack
import Coal.Compiler.State (CompilerState (compilerCurrentPath))
import Coal.Language (Expression (..), Pattern (..))
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Coal.Language.Type.Operations (maxTupleArity)
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Data.Generics.Uniplate.Data (universeBi)
import Extras (traverse_)

{- | Tuple arity validation pass.

Scans a module for tuple literals ('ETuple') and tuple patterns ('PTuple')
whose element count exceeds 'maxTupleArity', reporting a 'TupleTooLarge' error
at the offending source location.
-}
passCheckTupleArity :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passCheckTupleArity = mapPass $ Pass{runPass = traverse passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
passImpl m = do
  setCurrentModuleC m
  (_, errors) <- listenErrors (checkTupleArity m)
  unless (null errors) $
    throwError PreflightFailure
  return m

checkTupleArity :: (Monad m) => Module Metadata () () -> CompilerT Metadata m ()
checkTupleArity m = do
  traverse_ checkExpr (universeBi m :: [Expression Metadata () ()])
  traverse_ checkPat (universeBi m :: [Pattern Metadata () ()])

checkExpr :: (Monad m) => Expression Metadata () () -> CompilerT Metadata m ()
checkExpr = \case
  ETuple loc _ es -> checkArity loc (length es)
  _ -> pure ()

checkPat :: (Monad m) => Pattern Metadata () () -> CompilerT Metadata m ()
checkPat = \case
  PTuple loc _ ps -> checkArity loc (length ps)
  _ -> pure ()

checkArity :: (Monad m) => Metadata -> Int -> CompilerT Metadata m ()
checkArity loc n
  | n > maxTupleArity = do
      path <- gets compilerCurrentPath
      tellErrors [TupleTooLarge n maxTupleArity (ErrorLocation (principalPath path) loc)]
  | otherwise = pure ()
