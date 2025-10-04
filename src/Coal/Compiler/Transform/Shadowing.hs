{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Shadowing where

import Coal.Common.FreeVars (BoundVars (..), notConstructor)
import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Name (isConstructor)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad (when)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Extra (Name, forM_)

-- Detect?
class ReportShadowing e where
  detect :: (Monad m, Data a) => Set Name -> e -> CompilerT a m e

instance (ReportShadowing e) => ReportShadowing [e] where
  detect = traverse . detect

instance (ReportShadowing e) => ReportShadowing (NonEmpty e) where
  detect = traverse . detect

instance (Data a, Data t) => ReportShadowing (Expression a t) where
  detect names =
    \case
      EAnnotation loc t e ->
        EAnnotation loc t <$> detect names e
      var@EVariable{} -> do
        pure var
      ELambda a ps e -> do
        names' <- addNames (boundIn ps) names
        ELambda a ps <$> detect names' e
      ELet a gs e1 -> do
        names' <- addNames (boundIn gs) names
        ELet a
          <$> detect names gs
          <*> detect names' e1
      ERecursiveLet a p e1 e2 -> do
        names' <- addNames (boundIn p) names
        ERecursiveLet a p
          <$> detect names' e1
          <*> detect names' e2
      ERecord a t d e ->
        ERecord a t
          <$> traverse (detect names) d
          <*> traverse (detect names) e
      ESelect a ll e ->
        ESelect a ll <$> detect names e
      EFocus field ll1 ll2 e1 e2 -> do
        EFocus field ll1 ll2 <$> detect names e1 <*> detect names e2
      EIf a t e1 e2 e3 ->
        EIf a t
          <$> detect names e1
          <*> detect names e2
          <*> detect names e3
      expr@ELiteral{} ->
        pure expr
      expr@EConstructor{} ->
        pure expr
      EApplication a t e1 es ->
        EApplication a t
          <$> detect names e1
          <*> traverse (detect names) es
      EListCons a t e1 e2 ->
        EListCons a t
          <$> detect names e1
          <*> detect names e2
      EMatch a t e cs ->
        EMatch a t
          <$> detect names e
          <*> traverse (detect names) cs
      ECompiledMatch a t e cs ->
        ECompiledMatch a t
          <$> detect names e
          <*> traverse (detect names) cs
      ELambdaMatch a t cs me ->
        ELambdaMatch a t
          <$> traverse (detect names) cs
          <*> traverse (detect names) me
      EFold a t es cs me ->
        EFold a t
          <$> traverse (detect names) es
          <*> traverse (detect names) cs
          <*> traverse (detect names) me
      ECodataSelect a ll e me ->
        ECodataSelect a ll
          <$> detect names e
          <*> traverse (detect names) me
      expr@EUnaryOperator{} ->
        pure expr
      expr@EBinaryOperator{} ->
        pure expr
      EListLiteral a t es ->
        EListLiteral a t <$> traverse (detect names) es
      ETuple a t es ->
        ETuple a t <$> traverse (detect names) es
      e ->
        error "TODO:"

instance (Data a, Data t) => ReportShadowing (Clause a t) where
  detect names =
    \case
      EClause a p cs -> do
        names' <- addNames (boundIn p) names
        EClause a p <$> detect names' cs

instance (Data a, Data t) => ReportShadowing (Choice Expression a t) where
  detect names =
    \case
      CPlain a gs e ->
        CPlain a <$> detect names gs <*> detect names e
      CLambda{} ->
        error "TODO"

instance (Data a, Data t) => ReportShadowing (Guard Expression a t) where
  detect names =
    \case
      CGuard e ->
        CGuard <$> detect names e

instance (Data a, Data t) => ReportShadowing (CompiledClause a t) where
  detect names =
    \case
      ECompiledClause lls e -> do
        names' <- addNames (boundIn lls) names
        ECompiledClause lls <$> detect names' e

instance (Data a, Data t) => ReportShadowing (Binding Expression a t) where
  detect names =
    \case
      BPattern a p e -> do
        names' <- addNames (boundIn p) names
        BPattern a p <$> detect names' e
      BFunction a n ps e -> do
        names' <- addNames (boundIn ps) names
        BFunction a n ps <$> detect names' e

instance (Data a, Data t) => ReportShadowing (Module a Kind t) where
  detect names =
    \case
      Module p ns o ->
        Module p ns <$> detect names o

instance (Data a, Data t) => ReportShadowing (Definition a k t) where
  detect names =
    \case
      DFunction loc name f fs -> do
        names' <- addNames (Set.singleton name) names
        DFunction loc name
          <$> detect names' f
          <*> detect names' fs
      DConstant loc name g fs -> do
        names' <- addNames (Set.singleton name) names
        DConstant loc name
          <$> detect names' g
          <*> detect names' fs
      o ->
        pure o

instance (Data a, Data t) => ReportShadowing (ConstantDef a t) where
  detect names =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> detect names e

instance (Data a, Data t) => ReportShadowing (FunctionDef a t) where
  detect names =
    \case
      FunctionDef a u w ps e -> do
        names' <- addNames (boundIn ps) names
        FunctionDef a u w ps <$> detect names' e

addNames :: (Monad m) => Set Name -> Set Name -> CompilerT a m (Set Name)
addNames new names = do
  forM_ new' $
    \name ->
      when (name `elem` names) $
        error ("Shadowing: " <> show name)
  pure (new' <> names)
 where
  new' = Set.filter (not . isConstructor) new
