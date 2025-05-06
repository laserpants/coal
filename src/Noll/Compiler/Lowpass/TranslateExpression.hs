{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateExpression (translateExpression, translatePattern) where

import Data.Maybe (fromMaybe)
import Data.Data (Data)
import Lang.Common.List1 (List1, NonEmpty (..))
import Lang.Label (Label (..))
import Noll.Ast.HasType (HasType (..))
import Noll.Compiler.Lowpass.TranslateType (translateType)
import Noll.Language
import Noll.Language.Expression
import Noll.Language.Expression.Binding
import Noll.Language.Expression.Operator.Binary
import Lang.Utils (Dictionary)
import Noll.Language.Expression.Operator.Unary
import Noll.Language.Pattern
import Noll.Language.Primitive

import qualified Lang.Lowpass.Language as Lowpass
import qualified Data.Map.Strict as Map

type LowpassExpr = Lowpass.Expr Lowpass.Type

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

translateExpression :: (Data a) => Expression a IndexedType -> LowpassExpr
translateExpression =
  \case
    EAnnotation _ _ e ->
      translateExpression e
    EApplication _ t (EUnaryOperator _ _ op) es ->
      undefined
    EApplication _ t (EBinaryOperator _ _ op) es ->
      translateBinaryOperator t op es
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
    ERecord _ t d me ->
      translateRecord t d me
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
    ESelect _ ll@(Label t name) e ->
      Lowpass.sel
        (Lowpass.Focus name v (Label Lowpass.opaque "_"))
        (translateExpression e)
        (Lowpass.var v)
       where
        v = translateLabel ll
    EFocus name0 ll1 ll2 e1 e2 ->
      Lowpass.sel
        (Lowpass.Focus name0 (translateLabel ll1) (translateLabel ll2))
        (translateExpression e1)
        (translateExpression e2)
    EDictionaryLambda a ts e ->
      undefined
    EDictionaryApplication a t e ts es ->
      undefined

translateRecord :: (Data a) => IndexedType -> Dictionary (Expression a IndexedType) -> Maybe (Expression a IndexedType) -> LowpassExpr
translateRecord t d me =
  Lowpass.app
    (translateType t)
    (Lowpass.var (Label (Lowpass.arrow t1 (Lowpass.TCon "record" [t1])) "$Record"))
    (e1 :| [])
  where
    exprs = translateExpression <$> d
    expr0 = translateExpression <$> me
    e1 = foldr (uncurry Lowpass.ext) (fromMaybe Lowpass.nil expr0) (Map.toList exprs)
    t1 = Lowpass.typeOf e1

translateBinding :: (Data a) => Binding Expression a IndexedType -> Lowpass.Binding Lowpass.Type LowpassExpr
translateBinding =
  \case
    BPattern _ (PVariable _ ll) e ->
      Lowpass.Binding (translateLabel ll) (translateExpression e)

translatePattern :: Pattern a IndexedType -> Label Lowpass.Type
translatePattern =
  \case
    PVariable a (Label t name) ->
      Label (translateType t) name
    PAnnotation _ _ p ->
      translatePattern p

translateClause :: (Data a) => CompiledClause a IndexedType -> Lowpass.Clause Lowpass.Type LowpassExpr
translateClause =
  \case
    ECompiledClause lls e ->
      Lowpass.Clause (translateLabel <$> lls) (translateExpression e)

{-# INLINE translateLabel #-}
translateLabel :: Label IndexedType -> Label Lowpass.Type
translateLabel (Label t name) = Label (translateType t) name

translateBinaryOperator :: (Data a) => IndexedType -> BinaryOperator -> List1 (Expression a IndexedType) -> LowpassExpr
translateBinaryOperator t =
  \case
    OReverseComposition ->
      reverseCompositionOperator t
    OReverseApplication ->
      reverseApplicationOperator t
    OLessThan ->
      binop Lowpass.OLtInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OGreaterThan ->
      binop Lowpass.OGtInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OLogicalAnd ->
      binop Lowpass.OAnd (TIntrinsic IBool, TIntrinsic IBool)
    OLogicalOr ->
      binop Lowpass.OOr (TIntrinsic IBool, TIntrinsic IBool)

reverseCompositionOperator :: (Data a) => IndexedType -> List1 (Expression a IndexedType) -> LowpassExpr
reverseCompositionOperator t es =
  Lowpass.app
    t1
    (Lowpass.var (Label (Lowpass.foldType t1 (Lowpass.typeOf <$> exprs)) "Prelude.operator__reverse_composition"))
    exprs
 where
  t1 = translateType t
  exprs = translateExpression <$> es

reverseApplicationOperator :: (Data a) => IndexedType -> List1 (Expression a IndexedType) -> LowpassExpr
reverseApplicationOperator t es =
  Lowpass.app
    t1
    (Lowpass.var (Label (Lowpass.foldType t1 (Lowpass.typeOf <$> exprs)) "Prelude.operator__reverse_application"))
    exprs
 where
  t1 = translateType t
  exprs = translateExpression <$> es


binop :: (Data a) => (LowpassExpr -> LowpassExpr -> Lowpass.Op LowpassExpr) -> (IndexedType, IndexedType) -> List1 (Expression a IndexedType) -> LowpassExpr
binop op (t1, t2) (e1 :| [e2])
  | e1 `hasType` t1 && e2 `hasType` t2 =
      Lowpass.op (op (translateExpression e1) (translateExpression e2))

{-# INLINE hasType #-}
hasType :: (Data a) => Expression a IndexedType -> IndexedType -> Bool
hasType e t = (typeOf e :: IndexedType) == t
