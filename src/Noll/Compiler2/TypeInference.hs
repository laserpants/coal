{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.TypeInference where

import Control.Monad.State (gets)
import Control.Monad.Reader (asks)
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
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment

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

typeDefinitionsC :: (Monad m, Data a) => [Definition a Kind IndexedType] -> Compiler2T a m ([Definition a Kind IndexedType], [CompilerAssumption])
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
      env <- asks compiler2TraitEnvironment
      case Environment.lookup trait env of
        Nothing ->
          error ("Missing trait: " <> Text.unpack trait)
        Just (tx, defs) -> do
          forM_ ds $ \d -> do
            case Environment.lookup (definitionName d) defs of
              Nothing ->
                error ("Missing implementation: " <> Text.unpack (definitionName d))
              Just s -> do
                ti <- instantiateVars t1
                insertConstraintsC [Explicit InferenceRulePlaceholder (typeOf d) (instantiateTemplate tx ti s)]
                compileDefinitionC d
    d -> do
      compileDefinitionC d
      sub <- solveC
      defineC (definitionName d) (typeOf (apply sub d))

instantiateTemplate :: TypeIndex Kind -> IndexedType -> Scheme TypeIndex Kind IndexedType -> Scheme TypeIndex Kind IndexedType
instantiateTemplate (TypeIndex _ n) t1 (Forall vs ts t) = Forall vs ts (apply (n `mapsTo` t1) t)

instantiateVars :: (Monad m) => Type Parameter Kind -> Compiler2T a m IndexedType
instantiateVars = do
  \case
    TVariable{} -> do
      supplied (TVariable . TypeIndex KType)
    TApplication k t ts ->
      TApplication k <$> instantiateVars t <*> traverse instantiateVars ts
    TArrow t1 t2 ->
      TArrow <$> instantiateVars t1 <*> instantiateVars t2
    TIntrinsic t ->
      TIntrinsic <$> traverse instantiateVars t
    TRow r ->
      TRow <$> instantiateRowVars r
    TAlias name ts t ->
      TAlias name <$> traverse instantiateVars ts <*> instantiateVars t
    _ ->
      error "TODO"

instantiateRowVars :: (Monad m) => Row Parameter Kind (Type Parameter Kind) -> Compiler2T a m (Row TypeIndex Kind IndexedType)
instantiateRowVars =
  \case
    RVariable{} ->
      supplied (RVariable . TypeIndex KType)
    RExtend name t r ->
      RExtend name <$> instantiateVars t <*> instantiateRowVars r
    RNil ->
      pure RNil

defineC :: (Monad m) => Name -> IndexedType -> Compiler2T a m ()
defineC name t = insertNameC name (Forall (typeIndexesIn s) [] s)
 where
  s = normalizeTypeIndexes t
