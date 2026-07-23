{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.Translate.Expression (
  translateExpression,
  translatePattern,
) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.Environment (qualifyName, withLocalName, withLocalNames)
import Coal.Compiler.Kernel.Translate.Operator (translateOperator)
import Coal.Compiler.Kernel.Translate.Pattern (translatePattern)
import Coal.Compiler.Kernel.Translate.Primitive (translatePrimitive)
import Coal.Compiler.Kernel.Translate.Record (makeRecord, translateRecord)
import Coal.Compiler.Kernel.Translate.Type (translateType)
import Coal.Compiler.Stack (CompilerT)
import qualified Coal.Kernel.Language.Expr as Kernel
import qualified Coal.Kernel.Language.Type as Kernel
import qualified Coal.Kernel.Language.Type.Constructors as Kernel
import qualified Coal.Kernel.Language.Type.HasType as Kernel
import Coal.Language
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Text as Text
import Extras (Name)
import TextShow (showt)

type NKExpr = Kernel.Expr Kernel.Type

translateExpression ::
  (Monad m, Data a) =>
  Expression a Kind IndexedType ->
  CompilerT a m NKExpr
translateExpression =
  \case
    EAnnotation _ _ e ->
      translateExpression e
    EApplication _ t (EOperator _ _ op) es ->
      translateOperator translateExpression t op es
    EApplication _ t e es ->
      Kernel.EApp (translateType t)
        <$> translateExpression e
        <*> traverse translateExpression es
    ELambda _ ps e -> do
      qs <- traverse translatePattern ps
      e1 <- withLocalNames (nkLabelName <$> toList qs) (translateExpression e)
      pure (Kernel.ELam qs e1)
    ELet _ vs e -> do
      ws <- traverse translateBinding vs
      let localNames = [nkLabelName lbl | Kernel.Binding lbl _ <- toList ws]
      d1 <- withLocalNames localNames (translateExpression e)
      pure (Kernel.ELet ws d1)
    ERecursiveLet _ (PVariable _ (Label t name)) e1 e2 -> do
      let kLabel = Kernel.Label (translateType t) name
      d1 <- withLocalName name (translateExpression e1)
      d2 <- withLocalName name (translateExpression e2)
      pure (Kernel.ELet (Kernel.Binding kLabel d1 :| []) d2)
    EVariable _ (Label t name) ->
      Kernel.EVar . Kernel.Label (translateType t) <$> qualifyName name
    EConstructor _ (Label t name) ->
      Kernel.ECon . Kernel.Label (translateType t) <$> qualifyName name
    ELiteral _ p ->
      pure (Kernel.ELit (translatePrimitive p))
    EIf _ _ e1 e2 e3 ->
      Kernel.EIf
        <$> translateExpression e1
        <*> translateExpression e2
        <*> translateExpression e3
    ERecord _ t d me ->
      translateRecord translateExpression t d me
    EListCons _ _ e1 e2 ->
      consNK <$> translateExpression e1 <*> translateExpression e2
    EListLiteral _ t [] ->
      pure (Kernel.ECon (Kernel.Label (translateType t) "$Nil"))
    EListLiteral a t (e : es) ->
      translateExpression (foldr (EListCons a t) (EListLiteral a t []) (e : es))
    ETuple _ _ es ->
      tupleExprNK <$> traverse translateExpression es
    ECompiledMatch _ t e cs ->
      Kernel.ECase (translateType t)
        <$> translateExpression e
        <*> traverse translateClause cs
    ESelect _ (Label t field) e -> do
      d1 <- translateExpression e
      let t1 =
            case Kernel.typeOf d1 of
              Kernel.TCon _ [r] -> r
              _ -> error "Implementation error"
      pure $
        Kernel.ECase
          (translateType t)
          d1
          ( Kernel.Clause
              (Kernel.Label (Kernel.TCon "record" [t1]) "$Record" :| [Kernel.Label t1 "$row"])
              (Kernel.EGet (Kernel.Label (translateType t) field) (Kernel.EVar (Kernel.Label t1 "$row")))
              :| []
          )
    -- Focus on a field of a record, binding both the extracted field value
    -- and the record tail (the original record re-boxed) for use in the body.
    EFocus _ name0 (Label ft1 n1) (Label ft2 n2) e1 e2 -> do
      recordExpr <- translateExpression e1
      bodyExpr <- withLocalNames [n1, n2] (translateExpression e2)

      let fieldType = translateType ft1
          tailType = translateType ft2

          -- Bind n1 to the value of the focused field projected from the record
          fieldBinding =
            Kernel.Binding
              (Kernel.Label fieldType n1)
              (Kernel.EGet (Kernel.Label fieldType name0) recordExpr)

          -- Bind n2 to the record tail (re-box the original record via $Record)
          tailBinding =
            Kernel.Binding
              (Kernel.Label tailType n2)
              (makeRecord tailType recordExpr)

      pure (Kernel.ELet (fieldBinding :| [tailBinding]) bodyExpr)
    ETraitInstance _ t trait ->
      pure (Kernel.EVar (Kernel.Label (translateType t) (dictionaryLabel trait)))
    EFFICall _ _ (Label t name) es e -> do
      translatedEs <- traverse translateExpression es
      body <- translateExpression e
      pure (Kernel.ECall (Kernel.Label (translateType t) name) translatedEs body)
    EMatch{} ->
      error "Implementation error"
    ELambdaMatch{} ->
      error "Implementation error"
    _ ->
      error "Not implemented"

translateBinding ::
  (Monad m, Data a) =>
  Binding Expression a Kind IndexedType ->
  CompilerT a m (Kernel.Binding Kernel.Type)
translateBinding =
  \case
    BPattern _ (PVariable _ (Label t name)) e -> do
      e1 <-
        withLocalNames
          [n | n <- [name], Text.isPrefixOf "$fold" n]
          (translateExpression e)
      pure (Kernel.Binding (Kernel.Label (translateType t) name) e1)
    _ ->
      error "Not implemented"

translateClause ::
  (Monad m, Data a) =>
  CompiledClause a Kind IndexedType ->
  CompilerT a m (Kernel.Clause Kernel.Type)
translateClause =
  \case
    ECompiledClause _ (ll :| lls) e -> do
      e1 <- withLocalNames (labelName <$> lls) (translateExpression e)
      ll0 <- qualifyLabel ll
      pure (Kernel.Clause (translateLabel <$> (ll0 :| lls)) e1)

{-# INLINE qualifyLabel #-}
qualifyLabel ::
  (Monad m) =>
  Label IndexedType ->
  CompilerT a m (Label IndexedType)
qualifyLabel (Label t name) = Label t <$> qualifyName name

{-# INLINE translateLabel #-}
translateLabel :: Label IndexedType -> Kernel.Label Kernel.Type
translateLabel (Label t name) = Kernel.Label (translateType t) name

{-# INLINE nkLabelName #-}
nkLabelName :: Kernel.Label t -> Name
nkLabelName (Kernel.Label _ name) = name

-- | Build a list cons cell in the new kernel.
consNK :: NKExpr -> NKExpr -> NKExpr
consNK x xs =
  Kernel.EApp
    (Kernel.TCon "list" [t])
    (Kernel.ECon (Kernel.Label (Kernel.arrow t (Kernel.arrow (Kernel.TCon "list" [t]) (Kernel.TCon "list" [t]))) "$Cons"))
    (x :| [xs])
 where
  t = Kernel.typeOf x

-- | Build a tuple expression in the new kernel.
tupleExprNK :: NonEmpty NKExpr -> NKExpr
tupleExprNK es =
  Kernel.EApp t (Kernel.ECon (Kernel.Label (Kernel.foldType t ts) ("$Tuple" <> showt n))) es
 where
  n = length es
  t = Kernel.TCon "tuple" (foldr (:) [] ts)
  ts = Kernel.typeOf <$> es
