{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Kernel.TranslateExpression (
  translateExpression,
  translatePattern,
) where

import Control.Monad.Reader (MonadReader)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Lang.Common.List1 (List1, NonEmpty (..), fromList1, (<|))
import Lang.Common.Label (Label (..))
import Extra (Dictionary, Name)
import Noll.Compiler.Kernel.Environment (KernelEnvironment (..), qualifyName, withLocalName, withLocalNames)
import Noll.Compiler.Kernel.TranslateType (translateType)
import Noll.Language

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lang.Lowpass.Language as Lowpass

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

translateExpression :: (MonadReader KernelEnvironment m, Data a) => Expression a IndexedType -> m LowpassExpr
translateExpression =
  \case
    EAnnotation _ _ e ->
      translateExpression e
    EApplication _ t (EUnaryOperator _ ot op) es ->
      error "TODO"
    EApplication _ t (EBinaryOperator _ ot op) es ->
      translateBinaryOperator t ot op es
    EApplication _ t e es ->
      Lowpass.app (translateType t)
        <$> translateExpression e
        <*> traverse translateExpression es
    ELambda _ ps e -> do
      qs <- traverse translatePattern ps
      e1 <- withLocalNames (labelName <$> fromList1 qs) (translateExpression e)
      pure (Lowpass.lam qs e1)
    ELet _ vs e -> do
      ws <- traverse translateBinding vs
      d1 <- withLocalNames (labelName . Lowpass.bindingLabel <$> ws) (translateExpression e)
      pure (Lowpass.let_ ws d1)
    ERecursiveLet _ (PVariable _ ll) e1 e2 -> do
      d1 <- withLocalName (labelName ll) (translateExpression e1)
      d2 <- withLocalName (labelName ll) (translateExpression e2)
      pure (Lowpass.let_ (Lowpass.Binding (translateLabel ll) d1 :| []) d2)
    EVariable _ (Label t name) -> do
      qq <- qualifyName name
      pure (Lowpass.var (Label (translateType t) qq))
    EConstructor _ (Label t name) -> do
      qq <- qualifyName name
      pure (Lowpass.var (Label (translateType t) qq))
    ELiteral _ p ->
      pure (Lowpass.lit (translatePrimitive p))
    EIf _ _ e1 e2 e3 ->
      Lowpass.if_
        <$> translateExpression e1
        <*> translateExpression e2
        <*> translateExpression e3
    ERecord _ t d me ->
      translateRecord t d me
    EListCons _ _ e1 e2 ->
      Lowpass.cons
        <$> translateExpression e1
        <*> translateExpression e2
    EListLiteral _ t [] ->
      pure (Lowpass.var (Label (translateType t) "$Nil"))
    EListLiteral a t (e : es) ->
      translateExpression (foldr (EListCons a t) (EListLiteral a t []) (e : es))
    ETuple _ _ es ->
      Lowpass.tupleExpr <$> traverse translateExpression es
    EMatch{} ->
      error "Implementation error"
    ECompiledMatch _ t e cs ->
      Lowpass.match (translateType t) <$> translateExpression e <*> traverse translateClause cs
    ESelect _ ll@(Label t field) e -> do
      d1 <- translateExpression e
      let
        r = extractRow d1
        t1 = Lowpass.typeOf r
      pure $
        Lowpass.match
          (translateType t)
          d1
          ( Lowpass.Clause
              (Label (Lowpass.arrow t1 (Lowpass.TCon "record" [r])) "$Record" <| Label t1 "$row" :| [])
              ( Lowpass.sel
                  (Lowpass.Focus field (translateLabel ll) (Label (Lowpass.dropField field r) "_"))
                  (Lowpass.var (Label r "$row"))
                  (Lowpass.var (translateLabel ll))
              )
              :| []
          )
    ECodataSelect _ ll@(Label t field) e me -> do
      d1 <- translateExpression e
      let r = extractRow d1
      pure
        ( Lowpass.sel
            (Lowpass.Focus field (translateLabel ll) (Label (Lowpass.dropField field r) "_"))
            (Lowpass.var (Label r "$row"))
            (Lowpass.var (translateLabel ll))
        )
    EFocus name0 ll1 ll2@(Label t1 _) e1 e2 -> do
      d1 <- translateExpression e1
      d2 <- withLocalNames [labelName ll1, labelName ll2] (translateExpression e2)
      pure $
        Lowpass.sel
          (Lowpass.Focus name0 (translateLabel ll1) (Label r "$rest"))
          d1
          ( Lowpass.let_
              ( Lowpass.Binding
                  (translateLabel ll2)
                  ( Lowpass.app
                      t
                      (Lowpass.var (Label (r `Lowpass.arrow` t) "$Record"))
                      (Lowpass.var (Label r "$rest") :| [])
                  )
                  :| []
              )
              d2
          )
     where
      t = translateType t1
      r = extractRow (translateLabel ll2)
    EPlaceholder _ t trait@(Trait name _) ->
      pure (Lowpass.var (Label (translateType t) (dictVariable name trait)))
    EFold _ _ _ _ (Just e) ->
      translateExpression e
    EUnfold _ _ _ _ _ _ (Just e) ->
      translateExpression e
    ECodataFields _ _ fields -> do
      exprs <- traverse translateExpression fields
      pure (foldr (uncurry Lowpass.ext) Lowpass.nil (Map.toList exprs))
    _ ->
      error "TODO"

extractRow :: (Lowpass.Typed a) => a -> Lowpass.Type
extractRow e =
  case Lowpass.typeOf e of
    Lowpass.TCon _ [r] ->
      r
    _ ->
      error "Implementation error"

translateRecord :: (MonadReader KernelEnvironment m, Data a) => IndexedType -> Dictionary (Expression a IndexedType) -> Maybe (Expression a IndexedType) -> m LowpassExpr
translateRecord t d me = do
  exprs <- traverse translateExpression d
  expr0 <- traverse translateExpression me
  let
    e1 = foldr (uncurry Lowpass.ext) (fromMaybe Lowpass.nil expr0) (Map.toList exprs)
    t1 = Lowpass.typeOf e1
  pure $
    Lowpass.app
      (translateType t)
      (Lowpass.var (Label (Lowpass.arrow t1 (Lowpass.TCon "record" [t1])) "$Record"))
      (e1 :| [])

translateBinding :: (MonadReader KernelEnvironment m, Data a) => Binding Expression a IndexedType -> m (Lowpass.Binding Lowpass.Type LowpassExpr)
translateBinding =
  \case
    BPattern _ (PVariable _ ll) e -> do
      e1 <- withLocalNames [name | name <- [labelName ll], Text.isPrefixOf "$fold" name] (translateExpression e)
      pure (Lowpass.Binding (translateLabel ll) e1)
    _ ->
      error "Not implemented"

translatePattern :: (MonadReader KernelEnvironment m, Data a) => Pattern a IndexedType -> m (Label Lowpass.Type)
translatePattern =
  \case
    PAny a t ->
      translatePattern (PVariable a (Label t "_"))
    PVariable _ (Label t name) ->
      pure (Label (translateType t) name)
    PAnnotation _ _ p ->
      translatePattern p
    PLiteral _ p ->
      pure (Label (translateType (typeOf p)) "_")
    PPlaceholder _ t trait@(Trait name _) ->
      pure (Label (translateType t) (dictVariable name trait))
    _ ->
      error "TODO"

dictVariable :: (Serializable t) => Name -> Trait t -> Name
dictVariable name trait = "$d_" <> name <> "__$instance_" <> serialize trait

translateClause :: (MonadReader KernelEnvironment m, Data a) => CompiledClause a IndexedType -> m (Lowpass.Clause Lowpass.Type LowpassExpr)
translateClause =
  \case
    ECompiledClause (ll :| lls) e -> do
      e1 <- withLocalNames (labelName <$> lls) (translateExpression e)
      ll0 <- qualifyLabel ll
      pure (Lowpass.Clause (translateLabel <$> (ll0 :| lls)) e1)

{-# INLINE qualifyLabel #-}
qualifyLabel :: (MonadReader KernelEnvironment m) => Label IndexedType -> m (Label IndexedType)
qualifyLabel (Label t name) = Label t <$> qualifyName name

{-# INLINE translateLabel #-}
translateLabel :: Label IndexedType -> Label Lowpass.Type
translateLabel (Label t name) = Label (translateType t) name

translateBinaryOperator ::
  (MonadReader KernelEnvironment m, Data a) =>
  IndexedType ->
  IndexedType ->
  BinaryOperator ->
  List1 (Expression a IndexedType) ->
  m LowpassExpr
translateBinaryOperator t ot =
  \case
    OReverseComposition ->
      reverseCompositionOperator t
    OReverseApplication ->
      reverseApplicationOperator t
    OListConcatenation ->
      listConcatenationOperator t
    OLessThan ->
      binop Lowpass.OLtInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OGreaterThan ->
      binop Lowpass.OGtInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OLogicalAnd ->
      binop Lowpass.OAnd (TIntrinsic IBool, TIntrinsic IBool)
    OLogicalOr ->
      binop Lowpass.OOr (TIntrinsic IBool, TIntrinsic IBool)
    OAddition
      | TIntrinsic IInt32 == t ->
          binop Lowpass.OAddInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OAddition
      | TIntrinsic IInt64 == t ->
          binop Lowpass.OAddInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    OAddition
      | TIntrinsic IFloat == t ->
          binop Lowpass.OAddFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    OAddition
      | TIntrinsic IDouble == t ->
          binop Lowpass.OAddDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    OAddition ->
      error "TODO"
    OSubtraction
      | TIntrinsic IInt32 == t ->
          binop Lowpass.OSubInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OSubtraction
      | TIntrinsic IInt64 == t ->
          binop Lowpass.OSubInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    OSubtraction
      | TIntrinsic IFloat == t ->
          binop Lowpass.OSubFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    OSubtraction
      | TIntrinsic IDouble == t ->
          binop Lowpass.OSubDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    OSubtraction ->
      error "TODO"
    OMultiplication
      | TIntrinsic IInt32 == t ->
          binop Lowpass.OMulInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OMultiplication
      | TIntrinsic IInt64 == t ->
          binop Lowpass.OMulInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    OMultiplication
      | TIntrinsic IFloat == t ->
          binop Lowpass.OMulFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    OMultiplication
      | TIntrinsic IDouble == t ->
          binop Lowpass.OMulDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    OMultiplication ->
      error "TODO"
    ODivision
      | TIntrinsic IInt32 == t ->
          binop Lowpass.ODivInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    ODivision
      | TIntrinsic IInt64 == t ->
          binop Lowpass.ODivInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    ODivision
      | TIntrinsic IFloat == t ->
          binop Lowpass.ODivFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    ODivision
      | TIntrinsic IDouble == t ->
          binop Lowpass.ODivDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    ODivision ->
      error "TODO"
    OStringConcatenation ->
      stringConcatenationOperator
    OEqualTo ->
      equalityOperator ot
    _ ->
      error "Not implemented"

equalityOperator :: (MonadReader KernelEnvironment m, Data a) => IndexedType -> List1 (Expression a IndexedType) -> m LowpassExpr
equalityOperator ot (e1 :| [e2]) = do
  o1 <- translateExpression e1
  o2 <- translateExpression e2
  case ot of
    (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool) ->
      pure (Lowpass.op (Lowpass.OEqInt32 o1 o2))
    (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IBool) ->
      pure (Lowpass.op (Lowpass.OEqInt64 o1 o2))
    (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IBool) ->
      pure (Lowpass.op (Lowpass.OEqFloat o1 o2))
    (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IBool) ->
      pure (Lowpass.op (Lowpass.OEqDouble o1 o2))
    _ ->
      error "Not implemented"
equalityOperator _ _ = error "Not implemented"

stringConcatenationOperator :: (MonadReader KernelEnvironment m, Data a) => List1 (Expression a IndexedType) -> m LowpassExpr
stringConcatenationOperator es = do
  args <- traverse translateExpression es
  let t1 = translateType (TIntrinsic IString)
  pure $
    Lowpass.app
      t1
      (Lowpass.var (Label (t1 `Lowpass.arrow` t1 `Lowpass.arrow` t1) "Core$.operator__string_concatenation"))
      args

listConcatenationOperator :: (MonadReader KernelEnvironment m, Data a) => IndexedType -> List1 (Expression a IndexedType) -> m LowpassExpr
listConcatenationOperator t es = do
  args <- traverse translateExpression es
  let t1 = translateType t
  pure $
    Lowpass.app
      t1
      (Lowpass.var (Label (t1 `Lowpass.arrow` t1 `Lowpass.arrow` t1) "Core$.operator__list_concatenation"))
      args

reverseCompositionOperator :: (MonadReader KernelEnvironment m, Data a) => IndexedType -> List1 (Expression a IndexedType) -> m LowpassExpr
reverseCompositionOperator t es = do
  args <- traverse translateExpression es
  let t1 = translateType t
  pure $
    Lowpass.app
      t1
      (Lowpass.var (Label (Lowpass.foldType t1 (Lowpass.typeOf <$> args)) "Core$.operator__reverse_composition"))
      args

reverseApplicationOperator :: (MonadReader KernelEnvironment m, Data a) => IndexedType -> List1 (Expression a IndexedType) -> m LowpassExpr
reverseApplicationOperator t es = do
  args <- traverse translateExpression es
  let t1 = translateType t
  pure $
    Lowpass.app
      t1
      (Lowpass.var (Label (Lowpass.foldType t1 (Lowpass.typeOf <$> args)) "Core$.operator__reverse_application"))
      args

binop :: (MonadReader KernelEnvironment m, Data a) => (LowpassExpr -> LowpassExpr -> Lowpass.Op LowpassExpr) -> (IndexedType, IndexedType) -> List1 (Expression a IndexedType) -> m LowpassExpr
binop op (t1, t2) (e1 :| [e2])
  | e1 `hasType` t1 && e2 `hasType` t2 = do
      o1 <- translateExpression e1
      o2 <- translateExpression e2
      pure (Lowpass.op (op o1 o2))
binop _ _ _ = error "Implementation error"

{-# INLINE hasType #-}
hasType :: (Data a) => Expression a IndexedType -> IndexedType -> Bool
hasType e t = (typeOf e :: IndexedType) == t
