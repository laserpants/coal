{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateExpression where

import Lang.Common.List1 (NonEmpty (..))
import Lang.Label (Label (..))
import Noll.Compiler.Lowpass.TranslateType (translateType)
import Noll.Language.Expression
import Noll.Language.Expression.Binding
import Noll.Language.Pattern
import Noll.Language.Primitive
import Noll.Language.Type

import qualified Lang.Lowpass.Language as Lowpass

translatePrimitive :: Primitive -> Lowpass.Prim
translatePrimitive =
  \case
    LUnit ->
      Lowpass.PUnit
    LBool bool ->
      Lowpass.PBool bool
    LInt32 int32 ->
      Lowpass.PInt32 int32
    LInt64 int64 ->
      Lowpass.PInt64 int64
    LBignum int ->
      Lowpass.PBignum int
    LFloat float ->
      Lowpass.PFloat float
    LDouble double ->
      Lowpass.PDouble double
    LChar chr ->
      Lowpass.PChar chr
    LString str ->
      Lowpass.PString str

translateExpression :: Expression a (Type o k) -> Lowpass.Expr Lowpass.Type
translateExpression =
  \case
    EAnnotation _ _ e ->
      translateExpression e
    EApplication _ t e es ->
      Lowpass.app (translateType t) (translateExpression e) (translateExpression <$> es)
    ELambda _ ps e ->
      Lowpass.lam (translatePattern <$> ps) (translateExpression e)
    ELet _ vs e ->
      Lowpass.let_ (translateBinding <$> vs) (translateExpression e)
    ERecursiveLet _ (PVariable _ ll) e1 e2 ->
      Lowpass.let_
        (Lowpass.Binding (translateLabel ll) (translateExpression e1) :| [])
        (translateExpression e2)
    EVariable _ ll ->
      Lowpass.var (translateLabel ll)
    EConstructor _ ll ->
      Lowpass.var (translateLabel ll)
    ELiteral _ p ->
      Lowpass.lit (translatePrimitive p)
    EIf _ _ e1 e2 e3 ->
      Lowpass.if_ (translateExpression e1) (translateExpression e2) (translateExpression e3)
    EUnaryOperator a t op ->
      undefined
    EBinaryOperator a t op ->
      undefined
    ERecord a t d me ->
      undefined
    EListCons a t e1 e2 ->
      Lowpass.cons (translateExpression e1) (translateExpression e2)
    EListLiteral a t (e : es) ->
      translateExpression (foldr (EListCons a t) e es)
    ETuple _ _ es ->
      Lowpass.tupleExpr (translateExpression <$> es)
    EMatch{} ->
      error "Implementation error"
    ECompiledMatch _ t e cs ->
      Lowpass.match (translateType t) (translateExpression e) (translateClause <$> cs)
    EFold _ _ _ _ (Just e) ->
      translateExpression e
    ESelect _ ll e ->
      error "TODO"
    EFocus name0 ll1 ll2 e1 e2 ->
      Lowpass.sel
        (Lowpass.Focus name0 (translateLabel ll1) (translateLabel ll2))
        (translateExpression e1)
        (translateExpression e2)
    EDictionaryLambda a ts e ->
      undefined
    EDictionaryApplication a t e ts es ->
      undefined

translateBinding :: Binding Expression a (Type o k) -> Lowpass.Binding Lowpass.Type (Lowpass.Expr Lowpass.Type)
translateBinding =
  \case
    BPattern _ (PVariable _ ll) e ->
      Lowpass.Binding (translateLabel ll) (translateExpression e)

translatePattern :: Pattern a (Type o k) -> Label Lowpass.Type
translatePattern =
  \case
    PVariable a (Label t name) ->
      Label (translateType t) name

translateClause :: CompiledClause a (Type o k) -> Lowpass.Clause Lowpass.Type (Lowpass.Expr Lowpass.Type)
translateClause =
  \case
    ECompiledClause lls e ->
      Lowpass.Clause (translateLabel <$> lls) (translateExpression e)

translateLabel :: Label (Type o k) -> Label Lowpass.Type
translateLabel (Label t name) = Label (translateType t) name
