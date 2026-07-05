{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.NewTranslate.Expression (
  translateExpression,
  translatePattern,
) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.Environment (qualifyName, withLocalName, withLocalNames)
import Coal.Compiler.Kernel.NewTranslate.Operator (translateOperator)
import Coal.Compiler.Kernel.NewTranslate.Pattern (translatePattern)
import Coal.Compiler.Kernel.NewTranslate.Primitive (translatePrimitive)
import Coal.Compiler.Kernel.NewTranslate.Record (translateRecord)
import Coal.Compiler.Kernel.NewTranslate.Type (translateType)
import Coal.Compiler.Stack (CompilerT)
import qualified Coal.Kernel.Language.Expr as NK
import qualified Coal.Kernel.Language.Type as NKT
import qualified Coal.Kernel.Language.Type.Constructors as NKC
import qualified Coal.Kernel.Language.Type.HasType as NKHT
import Coal.Language
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Extras (Name)
import TextShow (showt)

type NKExpr = NK.Expr NKT.Type

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
      NK.EApp (translateType t)
        <$> translateExpression e
        <*> traverse translateExpression es
    ELambda _ ps e -> do
      qs <- traverse translatePattern ps
      e1 <- withLocalNames (nkLabelName <$> toList qs) (translateExpression e)
      pure (NK.ELam qs e1)
    ELet _ vs e -> do
      ws <- traverse translateBinding vs
      let localNames = [nkLabelName lbl | NK.Binding lbl _ <- toList ws]
      d1 <- withLocalNames localNames (translateExpression e)
      pure (NK.ELet ws d1)
    ERecursiveLet _ (PVariable _ (Label t name)) e1 e2 -> do
      let kLabel = NK.Label (translateType t) name
      d1 <- withLocalName name (translateExpression e1)
      d2 <- withLocalName name (translateExpression e2)
      pure (NK.ELet (NK.Binding kLabel d1 :| []) d2)
    EVariable _ (Label t name) ->
      NK.EVar . NK.Label (translateType t) <$> qualifyName name
    EConstructor _ (Label t name) ->
      NK.ECon . NK.Label (translateType t) <$> qualifyName name
    ELiteral _ p ->
      pure (NK.ELit (translatePrimitive p))
    EIf _ _ e1 e2 e3 ->
      NK.EIf
        <$> translateExpression e1
        <*> translateExpression e2
        <*> translateExpression e3
    ERecord _ t d me ->
      translateRecord translateExpression t d me
    EListCons _ _ e1 e2 ->
      consNK <$> translateExpression e1 <*> translateExpression e2
    EListLiteral _ t [] ->
      pure (NK.ECon (NK.Label (translateType t) "$Nil"))
    EListLiteral a t (e : es) ->
      translateExpression (foldr (EListCons a t) (EListLiteral a t []) (e : es))
    ETuple _ _ es ->
      tupleExprNK <$> traverse translateExpression es
    ECompiledMatch _ t e cs ->
      NK.ECase (translateType t)
        <$> translateExpression e
        <*> traverse translateClause cs
    ESelect _ (Label t field) e -> do
      d1 <- translateExpression e
      let t1 = case NKHT.typeOf d1 of
            NKT.TCon _ [r] -> r
            _ -> error "Implementation error"
      pure $
        NK.ECase
          t1
          d1
          ( NK.Clause
              (NK.Label (NKT.TCon "record" [t1]) "$Record" :| [NK.Label t1 "$row"])
              (NK.EGet (NK.Label (translateType t) field) (NK.EVar (NK.Label t1 "$row")))
              :| []
          )
    EFocus _ name0 (Label ft1 n1) (Label ft2 n2) e1 e2 -> do
      d1 <- translateExpression e1
      d2 <- withLocalNames [n1, n2] (translateExpression e2)
      let kft1 = translateType ft1
          kft2 = translateType ft2

          fieldBinding = NK.Binding (NK.Label kft1 n1) (NK.EGet (NK.Label kft1 name0) d1)
          tailBinding = NK.Binding (NK.Label kft2 n2) d1

      --          t1 =
      --            case NKHT.typeOf d1 of
      --              NKT.TCon _ [r] ->
      --                r
      --              q ->
      --                -- TODO: should throw error
      --                q -- error (show q) -- "Implementation error"
      --          unwrapped =
      --            NK.ECase
      --              t1
      --              d1
      --              ( NK.Clause
      --                  (NK.Label (NKT.TCon "record" [t1]) "$Record" :| [NK.Label t1 "$row"])
      --                  (NK.EVar (NK.Label t1 "$row"))
      --                  :| []
      --              )
      --          fieldBinding = NK.Binding (NK.Label kft1 n1) (NK.EGet (NK.Label kft1 name0) unwrapped)
      --          tailBinding = NK.Binding (NK.Label kft2 n2) unwrapped
      pure (NK.ELet (fieldBinding :| [tailBinding]) d2)
    ETraitInstance _ t trait ->
      pure (NK.EVar (NK.Label (translateType t) (dictionaryLabel trait)))
    EFFICall _ _ (Label t name) es e -> do
      translatedEs <- traverse translateExpression es
      body <- translateExpression e
      let callT = translateType t
      case NonEmpty.nonEmpty translatedEs of
        Nothing ->
          pure body
        Just args ->
          let fnType = NKHT.foldType callT (NKHT.typeOf <$> args)
           in pure $
                NK.ELet
                  (NK.Binding (NK.Label callT "_") (NK.EApp callT (NK.EVar (NK.Label fnType name)) args) :| [])
                  body
    EMatch{} ->
      error "Implementation error"
    ELambdaMatch{} ->
      error "Implementation error"
    _ ->
      error "Not implemented"

translateBinding ::
  (Monad m, Data a) =>
  Binding Expression a Kind IndexedType ->
  CompilerT a m (NK.Binding NKT.Type)
translateBinding =
  \case
    BPattern _ (PVariable _ (Label t name)) e -> do
      e1 <-
        withLocalNames
          [n | n <- [name], Text.isPrefixOf "$fold" n]
          (translateExpression e)
      pure (NK.Binding (NK.Label (translateType t) name) e1)
    _ ->
      error "Not implemented"

translateClause ::
  (Monad m, Data a) =>
  CompiledClause a Kind IndexedType ->
  CompilerT a m (NK.Clause NKT.Type)
translateClause =
  \case
    ECompiledClause _ (ll :| lls) e -> do
      e1 <- withLocalNames (labelName <$> lls) (translateExpression e)
      ll0 <- qualifyLabel ll
      pure (NK.Clause (translateLabel <$> (ll0 :| lls)) e1)

{-# INLINE qualifyLabel #-}
qualifyLabel ::
  (Monad m) =>
  Label IndexedType ->
  CompilerT a m (Label IndexedType)
qualifyLabel (Label t name) = Label t <$> qualifyName name

{-# INLINE translateLabel #-}
translateLabel :: Label IndexedType -> NK.Label NKT.Type
translateLabel (Label t name) = NK.Label (translateType t) name

{-# INLINE nkLabelName #-}
nkLabelName :: NK.Label t -> Name
nkLabelName (NK.Label _ name) = name

-- | Build a list cons cell in the new kernel.
consNK :: NKExpr -> NKExpr -> NKExpr
consNK x xs =
  NK.EApp
    (NKT.TCon "list" [t])
    (NK.ECon (NK.Label (NKC.arrow t (NKC.arrow (NKT.TCon "list" [t]) (NKT.TCon "list" [t]))) "$Cons"))
    (x :| [xs])
 where
  t = NKHT.typeOf x

-- | Build a tuple expression in the new kernel.
tupleExprNK :: NonEmpty NKExpr -> NKExpr
tupleExprNK es =
  NK.EApp t (NK.ECon (NK.Label (NKHT.foldType t ts) ("$Tuple" <> showt n))) es
 where
  n = length es
  t = NKT.TCon "tuple" (foldr (:) [] ts)
  ts = NKHT.typeOf <$> es
