{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.TypeInference where

import Control.Monad.State (gets)
import Data.Data (Data)
import Lang.Common.Environment (Environment (..))
import Lang.Common.List1 (NonEmpty (..))
import Lang.Common.Supply (supplied)
import Lang.Label (Label (..))
import Lang.Utils (Name, forM_)
import Noll.Compiler2.Internal
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..))
import Noll.Module.Definition (definitionName)
import Noll.SystemF

import qualified Data.Map.Strict as Map

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

solveC :: (Monad m) => Compiler2T a m Substitution
solveC = do
  constraints <- gets compiler2Constraints
  sub1 <- gets compiler2Substitution
  sub2 <- solveConstraintsC constraints
  clearConstraintsC
  -- TODO: Clear typeAnnotationParameters?
  updateSubstitutionC (sub2 <> sub1)
  gets compiler2Substitution

typeDefinitionsC :: (Monad m, Data a, Show a, Eq a) => [Definition a Kind IndexedType] -> Compiler2T a m ([Definition a Kind IndexedType], [CompilerAssumption])
typeDefinitionsC ds = do
  forM_ ds typeDefinitionC
  sub <- gets compiler2Substitution
  ams <- gets compiler2Assumptions
  Environment env <- gets compiler2NameStore
  insertConstraintsC $ do
    (n1, s) <- Map.toList env
    Assumption n2 t <- ams
    [Explicit InferenceRulePlaceholder (apply sub t) s | n1 == n2]
  sub1 <- solveC
  pure (fmap (fmap normalizeRowTypes) (apply sub1 ds), apply sub1 ams)

typeDefinitionC :: (Monad m, Data a) => Definition a Kind IndexedType -> Compiler2T a m ()
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
      undefined -- defineC (definitionName d) (typeOf (apply sub d))

defineC :: (Monad m) => Name -> IndexedType -> Compiler2T a m ()
defineC name t = insertNameC name (Forall (typeIndexesIn s) [] s)
 where
  s = normalizeTypeIndexes t
