{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.TypeInference where

import Data.Data (Data)
import Lang.Common.List1 (NonEmpty (..))
import Lang.Common.Supply (Supply (..), supplied)
import Control.Monad.State (gets)
import Lang.Label (Label (..))
import Noll.Compiler2.Internal
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..))
import Noll.SystemF

type CompilerAssumption = Assumption IndexedType

solveConstraintsC =
  undefined

compileConstraintsC =
  undefined

compileFunctionC :: (Monad m, Data a) => Function Expression a IndexedType -> Compiler2T a m ()
compileFunctionC (Function loc (With _ t) ps e) = do
  insertConstraintsC [Equality (RuleTopLevelFunction loc) [t, typeOf e]]
  t1 <- supplied (TVariable . TypeIndex KType)
  compileConstraintsC $
    ELet
      loc
      (BFunction loc placeholder ps e :| [])
      (EVariable loc (Label (foldTypeOf t1 ps) placeholder))
 where
  placeholder = "###.function"

compileConstantC :: (Monad m, Data a) => Constant Expression a IndexedType -> Compiler2T a m ()
compileConstantC (Constant loc (With _ t) e) = do
  insertConstraintsC [Equality (RuleTopLevelConstant loc) [t, typeOf e]]
  compileConstraintsC $
    ELet
      loc
      (BPattern loc (PVariable loc (Label t placeholder)) e :| [])
      (EVariable loc (Label t placeholder))
 where
  placeholder = "###.constant"

compileDefinitionC :: (Monad m, Data a) => Definition a k IndexedType -> Compiler2T a m ()
compileDefinitionC = do
  \case
    DFunction _ f ->
      compileFunctionC f
    DConstant _ c ->
      compileConstantC c
    _ ->
      error "TODO"

solveC :: (Monad m, Data a, Show a, Eq a) => Compiler2T a m Substitution
solveC = do
  constraints <- gets compiler2Constraints
  sub1 <- gets compiler2Substitution
  sub2 <- solveConstraintsC constraints
  clearConstraintsC
  -- TODO: Clear typeAnnotationParameters?
  updateSubstitutionC (sub2 <> sub1)
  gets compiler2Substitution

typeDefinitionsC :: (Monad m) => [Definition a k IndexedType] -> Compiler2T b m ([Definition a Kind IndexedType], [CompilerAssumption])
typeDefinitionsC = undefined

typeDefinitionC :: (Monad m, Data a, Show a, Eq a) => Definition a k IndexedType -> Compiler2T a m ()
typeDefinitionC =
  \case
    DImport{} ->
      pure ()
    DTrait{} ->
      pure ()
    DTypeAlias{} ->
      pure ()
    DType{} ->
      pure ()
    DCodata{} ->
      pure ()
    DSignature{} ->
      pure ()
    DInstance trait t1 ds -> do
      error "TODO"
    d -> do
      compileDefinitionC d
      sub <- solveC
      error "TODO"

  -- defineC (definitionName d) (typeOf (apply sub d))
