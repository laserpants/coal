{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateExpression where

import Lang.Label (Label (..))
import Noll.Compiler.Lowpass.TranslateType (translateType)
import Noll.Language.Expression
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
    LString text ->
      Lowpass.PString undefined -- TOOD text

translateExpression :: Expression a (Type o k) -> Lowpass.Expr Lowpass.Type
translateExpression =
  \case
    EAnnotation _ _ e ->
      translateExpression e
    --    EApplication a t (Expression a t) (List1 (Expression a t))
    --    ELambda a (List1 (Pattern a t)) (Expression a t)
    --    ELet a (List1 (Binding Expression a t)) (Expression a t)
    --    ERecursiveLet a (Pattern a t) (Expression a t) (Expression a t)
    EVariable _ (Label t name) ->
      Lowpass.var (Label (translateType t) name)
    EConstructor _ (Label t name) ->
      undefined
    ELiteral _ p ->
      Lowpass.lit (translatePrimitive p)
    EIf _ _ e1 e2 e3 ->
      Lowpass.if_
        (translateExpression e1)
        (translateExpression e2)
        (translateExpression e3)
    --    EUnaryOperator a t UnaryOperator
    --    EBinaryOperator a t BinaryOperator
    --    ERecord a t (Dictionary (Expression a t)) (Maybe (Expression a t))
    EListCons a t e1 e2 ->
      Lowpass.cons (translateExpression e1) (translateExpression e2)
    --    EListLiteral a t [Expression a t]
    ETuple _ _ es ->
      Lowpass.tupleExpr (translateExpression <$> es)
    EMatch{} ->
      error "Implementation error"
    ECompiledMatch _ t e cs ->
      Lowpass.match
        (translateType t)
        (translateExpression e)
        undefined
    EFold _ _ _ _ (Just e) ->
      translateExpression e

--    ESelect a (Label t) (Expression a t)
--    EFocus Name (Label t) (Label t) (Expression a t) (Expression a t)
--    EDictionaryLambda a (List1 (Trait t)) (Expression a t)
--    EDictionaryApplication a t (Expression a t) (List1 (Trait t)) [Expression a t]
