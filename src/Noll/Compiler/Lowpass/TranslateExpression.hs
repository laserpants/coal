{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateExpression (translateExpression, translatePattern) where

import Control.Monad.Reader (MonadReader, asks, local)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)
import Lang.Common.List1 (List1, NonEmpty (..), fromList1, (<|))
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Name, Set)
import Noll.Compiler.Lowpass.Environment (TranslateEnvironment (..), qualifyName, withLocalName, withLocalNames)
import Noll.Compiler.Lowpass.TranslateType (translateType)
import Noll.Language
import Noll.Language.Expression
import Noll.Language.Expression.Binding
import Noll.Language.Expression.Operator
import Noll.Language.HasType (HasType (..))
import Noll.Language.Pattern
import Noll.Language.Primitive
import Noll.Language.Serializable (serialize)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

translateExpression :: (MonadReader TranslateEnvironment m, Data a) => Expression a IndexedType -> m LowpassExpr
translateExpression =
  \case
    EAnnotation _ _ e ->
      translateExpression e
    EApplication _ t (EUnaryOperator _ _ op) es ->
      undefined
    EApplication _ t (EBinaryOperator _ _ op) es ->
      translateBinaryOperator t op es
    EApplication _ t e es -> do
      xx <- translateExpression e
      xs1 <- traverse translateExpression es
      pure (Lowpass.app (translateType t) xx xs1)
    --      Lowpass.app (translateType t) (translateExpression e) (translateExpression <$> es)
    ELambda _ ps e -> do
      qqs1 <- traverse translatePattern ps
      -- (Set.insert (labelName <$> qqs1))
      --      let sss1 = Set.fromList (labelName <$> fromList1 qqs1)
      --      xx1 <- local (Set.union sss1) (translateExpression e)
      xx1 <- withLocalNames (labelName <$> fromList1 qqs1) (translateExpression e)

      pure (Lowpass.lam qqs1 xx1)
    --      Lowpass.lam (translatePattern <$> ps) (translateExpression e)
    ELet _ vs e -> do
      vvs1 <- traverse translateBinding vs
      -- let sss1 = Set.fromList (fromList1 (labelName . Lowpass.bindingLabel <$> vvs1))
      -- xx1 <- local (Set.union sss1) (translateExpression e)

      xx1 <- withLocalNames (labelName . Lowpass.bindingLabel <$> vvs1) (translateExpression e)
      pure (Lowpass.let_ vvs1 xx1)
    --      Lowpass.let_ (translateBinding <$> vs) (translateExpression e)
    ERecursiveLet _ (PVariable _ ll) e1 e2 -> do
      xx1 <- withLocalName (labelName ll) (translateExpression e1)
      -- xx2 <- local (Set.insert (labelName ll)) (translateExpression e2)

      xx2 <- withLocalName (labelName ll) (translateExpression e2)
      pure (Lowpass.let_ (Lowpass.Binding (translateLabel ll) xx1 :| []) xx2)
    --      Lowpass.let_
    --        (Lowpass.Binding (translateLabel ll) (translateExpression e1) :| [])
    --        (translateExpression e2)
    EVariable _ (Label t name) -> do
      qq <- qualifyName name
      pure (Lowpass.var (Label (translateType t) qq))
    EConstructor _ (Label t name) -> do
      qq <- qualifyName name
      pure (Lowpass.var (Label (translateType t) qq))
    ELiteral _ p ->
      pure (Lowpass.lit (translatePrimitive p))
    EIf _ _ e1 e2 e3 -> do
      xx1 <- translateExpression e1
      xx2 <- translateExpression e2
      xx3 <- translateExpression e3
      pure (Lowpass.if_ xx1 xx2 xx3)
    -- Lowpass.if_ (translateExpression e1) (translateExpression e2) (translateExpression e3)
    EUnaryOperator a t op ->
      undefined
    EBinaryOperator a t op ->
      undefined
    ERecord _ t d me ->
      translateRecord t d me
    EListCons a t e1 e2 -> do
      xx1 <- translateExpression e1
      xx2 <- translateExpression e2
      pure (Lowpass.cons xx1 xx2)
    --      Lowpass.cons (translateExpression e1) (translateExpression e2)
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
      xx1 <- translateExpression e
      let
        Lowpass.TCon _ [r] = Lowpass.typeOf xx1
        t1 = Lowpass.typeOf r
      pure $
        Lowpass.match
          (translateType t)
          xx1
          ( Lowpass.Clause
              (Label (Lowpass.arrow t1 (Lowpass.TCon "record" [r])) "$Record" <| Label t1 "$row" :| [])
              ( Lowpass.sel
                  (Lowpass.Focus field (translateLabel ll) (Label (Lowpass.dropField field r) "_"))
                  (Lowpass.var (Label r "$row"))
                  (Lowpass.var (translateLabel ll))
              )
              :| []
          )
    ECodataSelect a ll@(Label t field) e me -> do
      -- TODO: DRY
      xx1 <- translateExpression e
      let
        Lowpass.TCon _ [r] = Lowpass.typeOf xx1
        t1 = Lowpass.typeOf r
      pure
        ( Lowpass.sel
            (Lowpass.Focus field (translateLabel ll) (Label (Lowpass.dropField field r) "_"))
            (Lowpass.var (Label r "$row"))
            (Lowpass.var (translateLabel ll))
        )
    EFocus name0 ll1 ll2 e1 e2 -> do
      xx1 <- translateExpression e1
      --      let sss1 = Set.fromList [labelName ll1, labelName ll2]
      --      xx2 <- local (Set.union sss1) (translateExpression e2)
      xx2 <- withLocalNames [labelName ll1, labelName ll2] (translateExpression e2)
      pure $
        Lowpass.sel
          (Lowpass.Focus name0 (translateLabel ll1) (Label r "$rest"))
          xx1
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
              xx2
          )
     where
      t@(Lowpass.TCon _ [r]) = Lowpass.typeOf (translateLabel ll2)

    --    -- Lowpass.sel
    --    --  (Lowpass.Focus name0 (translateLabel ll1) (translateLabel ll2))
    --    --  (translateExpression e1)
    --    --  (translateExpression e2)
    --    EPlaceholderLambda a ts e ->
    --      undefined
    --    EPlaceholderApplication a t e ts es ->
    --      undefined
    EPlaceholder _ t trait@(Trait name _) ->
      pure (Lowpass.var (Label (translateType t) ("$d_" <> name <> "__$instance_" <> serialize trait)))
    EFold _ _ _ _ (Just e) ->
      translateExpression e
    EUnfold _ _ _ _ _ _ (Just e) ->
      translateExpression e
    ECodataFields _ _ fields -> do
      exprs <- traverse translateExpression fields
      let e1 = foldr (uncurry Lowpass.ext) Lowpass.nil (Map.toList exprs)
      pure e1
    _ ->
      error "TODO"

--    t1 = Lowpass.typeOf e1
--    t = TIntrinsic IVoid -- TODO
-- pure $
--  Lowpass.app
--    (translateType t)
--    (Lowpass.var (Label (Lowpass.arrow t1 (Lowpass.TCon "record" [t1])) "$Record"))
--    (e1 :| [])

translateRecord :: (MonadReader TranslateEnvironment m, Data a) => IndexedType -> Dictionary (Expression a IndexedType) -> Maybe (Expression a IndexedType) -> m LowpassExpr
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

translateBinding :: (MonadReader TranslateEnvironment m, Data a) => Binding Expression a IndexedType -> m (Lowpass.Binding Lowpass.Type LowpassExpr)
translateBinding =
  \case
    BPattern _ (PVariable _ ll) e -> do
      xx1 <- withLocalNames [name | name <- [labelName ll], Text.isPrefixOf "$fold" name] (translateExpression e)
      pure (Lowpass.Binding (translateLabel ll) xx1)

translatePattern :: (MonadReader TranslateEnvironment m, Data a) => Pattern a IndexedType -> m (Label Lowpass.Type)
translatePattern =
  \case
    PAny a t ->
      translatePattern (PVariable a (Label t "_"))
    PVariable a (Label t name) ->
      pure (Label (translateType t) name)
    PAnnotation _ _ p ->
      translatePattern p
    PLiteral _ p ->
      pure (Label (translateType (typeOf p)) "_")
    PPlaceholder _ t trait@(Trait name _) ->
      -- TODO: DRY?
      pure (Label (translateType t) ("$d_" <> name <> "__$instance_" <> serialize trait))
    _ ->
      error "TODO"

translateClause :: (MonadReader TranslateEnvironment m, Data a) => CompiledClause a IndexedType -> m (Lowpass.Clause Lowpass.Type LowpassExpr)
translateClause =
  \case
    ECompiledClause (ll :| lls) e -> do
      xx1 <- withLocalNames (labelName <$> lls) (translateExpression e)
      qq <- qualifyLabel ll
      pure (Lowpass.Clause (translateLabel <$> (qq :| lls)) xx1)

qualifyLabel :: (MonadReader TranslateEnvironment m) => Label IndexedType -> m (Label IndexedType)
qualifyLabel (Label t name) = do
  qq <- qualifyName name
  pure (Label t qq)

{-# INLINE translateLabel #-}
translateLabel :: Label IndexedType -> Label Lowpass.Type
translateLabel (Label t name) = Label (translateType t) name

translateBinaryOperator :: (MonadReader TranslateEnvironment m, Data a) => IndexedType -> BinaryOperator -> List1 (Expression a IndexedType) -> m LowpassExpr
translateBinaryOperator t =
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
      binop Lowpass.OEqInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OEqualTo ->
      binop Lowpass.OEqInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    OEqualTo ->
      binop Lowpass.OEqFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    OEqualTo ->
      binop Lowpass.OEqDouble (TIntrinsic IDouble, TIntrinsic IDouble)

stringConcatenationOperator es = do
  args <- traverse translateExpression es
  let t1 = translateType (TIntrinsic IString)
  pure $
    Lowpass.app
      t1
      (Lowpass.var (Label (t1 `Lowpass.arrow` t1 `Lowpass.arrow` t1) "Core$.operator__string_concatenation"))
      args

listConcatenationOperator t es = do
  args <- traverse translateExpression es
  let t1 = translateType t
  pure $
    Lowpass.app
      t1
      (Lowpass.var (Label (t1 `Lowpass.arrow` t1 `Lowpass.arrow` t1) "Core$.operator__list_concatenation"))
      args

reverseCompositionOperator :: (MonadReader TranslateEnvironment m, Data a) => IndexedType -> List1 (Expression a IndexedType) -> m LowpassExpr
reverseCompositionOperator t es = do
  args <- traverse translateExpression es
  let t1 = translateType t
  pure $
    Lowpass.app
      t1
      (Lowpass.var (Label (Lowpass.foldType t1 (Lowpass.typeOf <$> args)) "Core$.operator__reverse_composition"))
      args

reverseApplicationOperator :: (MonadReader TranslateEnvironment m, Data a) => IndexedType -> List1 (Expression a IndexedType) -> m LowpassExpr
reverseApplicationOperator t es = do
  args <- traverse translateExpression es
  let t1 = translateType t
  pure $
    Lowpass.app
      t1
      (Lowpass.var (Label (Lowpass.foldType t1 (Lowpass.typeOf <$> args)) "Core$.operator__reverse_application"))
      args

binop :: (MonadReader TranslateEnvironment m, Data a) => (LowpassExpr -> LowpassExpr -> Lowpass.Op LowpassExpr) -> (IndexedType, IndexedType) -> List1 (Expression a IndexedType) -> m LowpassExpr
binop op (t1, t2) (e1 :| [e2])
  | e1 `hasType` t1 && e2 `hasType` t2 = do
      xx1 <- translateExpression e1
      xx2 <- translateExpression e2
      pure (Lowpass.op (op xx1 xx2))

{-# INLINE hasType #-}
hasType :: (Data a) => Expression a IndexedType -> IndexedType -> Bool
hasType e t = (typeOf e :: IndexedType) == t
