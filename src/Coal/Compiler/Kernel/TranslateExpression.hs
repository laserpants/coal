{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.TranslateExpression (
  translateExpression,
  translatePattern,
) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.Environment (KernelEnvironment (..), qualifyName, withLocalName, withLocalNames)
import Coal.Compiler.Kernel.TranslateType (translateType)
import Coal.Language
import Control.Monad.Reader (MonadReader)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), toList, (<|))
import Extra (Name)

import qualified Coal.Kernel.Language as Kernel
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

type KernelExpr = Kernel.Expr Kernel.Type

translatePrimitive :: Primitive -> Kernel.Prim
translatePrimitive =
  \case
    LUnit ->
      Kernel.PUnit
    LBool bool ->
      Kernel.PBool bool
    LInt32 int32 ->
      Kernel.PInt32 int32
    LInt64 int64 ->
      Kernel.PInt64 int64
    LBignum int ->
      Kernel.PBignum int
    LFloat float ->
      Kernel.PFloat float
    LDouble double ->
      Kernel.PDouble double
    LChar chr ->
      Kernel.PChar chr
    LString str ->
      Kernel.PString str

translateExpression :: (MonadReader KernelEnvironment m, Data a) => Expression a IndexedType -> m KernelExpr
translateExpression =
  \case
    EAnnotation _ _ e ->
      translateExpression e
    EApplication _ t (EUnaryOperator _ ot op) es ->
      error "TODO"
    EApplication _ t (EBinaryOperator _ ot op) es ->
      translateBinaryOperator t ot op es
    EApplication _ t e es ->
      Kernel.app (translateType t)
        <$> translateExpression e
        <*> traverse translateExpression es
    ELambda _ ps e -> do
      qs <- traverse translatePattern ps
      e1 <- withLocalNames (labelName <$> toList qs) (translateExpression e)
      pure (Kernel.lam qs e1)
    ELet _ vs e -> do
      ws <- traverse translateBinding vs
      d1 <- withLocalNames (labelName . Kernel.bindingLabel <$> ws) (translateExpression e)
      pure (Kernel.let_ ws d1)
    ERecursiveLet _ (PVariable _ ll) e1 e2 -> do
      d1 <- withLocalName (labelName ll) (translateExpression e1)
      d2 <- withLocalName (labelName ll) (translateExpression e2)
      pure (Kernel.let_ (Kernel.Binding (translateLabel ll) d1 :| []) d2)
    EVariable _ (Label t name) -> do
      qq <- qualifyName name
      pure (Kernel.var (Label (translateType t) qq))
    EConstructor _ (Label t name) -> do
      qq <- qualifyName name
      pure (Kernel.var (Label (translateType t) qq))
    ELiteral _ p ->
      pure (Kernel.lit (translatePrimitive p))
    EIf _ _ e1 e2 e3 ->
      Kernel.if_
        <$> translateExpression e1
        <*> translateExpression e2
        <*> translateExpression e3
    ERecord _ t d me -> do
      exprs <- traverse translateExpression d
      expr0 <- traverse translateExpression me
      let e2 =
            case expr0 of
              Nothing ->
                Kernel.nil
              Just e1 -> do
                let t1 = extractRow e1
                Kernel.match
                  t1
                  e1
                  ( Kernel.Clause
                      (Label (Kernel.TCon "record" [t1]) "$Record" :| [Label t1 "$row"])
                      (Kernel.var (Label t1 "$row"))
                      :| []
                  )
      pure $
        makeRecord
          (translateType t)
          (foldr (uncurry Kernel.ext) e2 (Map.toList exprs))
    EListCons _ _ e1 e2 ->
      Kernel.cons
        <$> translateExpression e1
        <*> translateExpression e2
    EListLiteral _ t [] ->
      pure (Kernel.var (Label (translateType t) "$Nil"))
    EListLiteral a t (e : es) ->
      translateExpression (foldr (EListCons a t) (EListLiteral a t []) (e : es))
    ETuple _ _ es ->
      Kernel.tupleExpr <$> traverse translateExpression es
    EMatch{} ->
      error "Implementation error"
    ECompiledMatch _ t e cs ->
      Kernel.match (translateType t) <$> translateExpression e <*> traverse translateClause cs
    ESelect _ ll@(Label t field) e -> do
      d1 <- translateExpression e
      let
        r = extractRow d1
        t1 = Kernel.typeOf r
      pure $
        Kernel.match
          (translateType t)
          d1
          ( Kernel.Clause
              (Label (Kernel.arrow t1 (Kernel.TCon "record" [r])) "$Record" <| Label t1 "$row" :| [])
              ( Kernel.sel
                  (Kernel.Focus field (translateLabel ll) (Label (Kernel.dropField field r) "_"))
                  (Kernel.var (Label r "$row"))
                  (Kernel.var (translateLabel ll))
              )
              :| []
          )
    ECodataSelect _ _ _ (Just e1) -> do
      translateExpression e1
    EFocus name0 ll1 ll2@(Label t1 _) e1 e2 -> do
      d1 <- translateExpression e1
      d2 <- withLocalNames [labelName ll1, labelName ll2] (translateExpression e2)
      pure $
        Kernel.sel
          (Kernel.Focus name0 (translateLabel ll1) (Label r "$rest"))
          d1
          ( Kernel.let_
              ( Kernel.Binding
                  (translateLabel ll2)
                  ( Kernel.app
                      t
                      (Kernel.var (Label (r `Kernel.arrow` t) "$Record"))
                      (Kernel.var (Label r "$rest") :| [])
                  )
                  :| []
              )
              d2
          )
     where
      t = translateType t1
      r = extractRow (translateLabel ll2)
    ETraitDictionary _ t trait@(Trait name _) ->
      pure (Kernel.var (Label (translateType t) (dictVariable name trait)))
    EFold _ _ _ _ _ (Just e) ->
      translateExpression e
    EUnfold _ _ _ _ _ (Just e) ->
      translateExpression e
    ECodataFields _ _ fields -> do
      exprs <- traverse translateExpression fields
      let r = foldr (uncurry Kernel.ext) Kernel.nil (Map.toList exprs)
      pure $ makeRecord (Kernel.TCon "record" [Kernel.typeOf r]) r
    _ ->
      error "TODO"

extractRow :: (Kernel.Typed a) => a -> Kernel.Type
extractRow e =
  case Kernel.typeOf e of
    Kernel.TCon _ [r] ->
      r
    _ ->
      error "Implementation error"

makeRecord :: Kernel.Type -> KernelExpr -> KernelExpr
makeRecord t e1 =
  Kernel.app
    t
    (Kernel.var (Label (Kernel.arrow t1 (Kernel.TCon "record" [t1])) "$Record"))
    (e1 :| [])
 where
  t1 = Kernel.typeOf e1

translateBinding :: (MonadReader KernelEnvironment m, Data a) => Binding Expression a IndexedType -> m (Kernel.Binding Kernel.Type KernelExpr)
translateBinding =
  \case
    BPattern _ (PVariable _ ll) e -> do
      e1 <- withLocalNames [name | name <- [labelName ll], Text.isPrefixOf "$fold" name] (translateExpression e)
      pure (Kernel.Binding (translateLabel ll) e1)
    _ ->
      error "Not implemented"

translatePattern :: (MonadReader KernelEnvironment m, Data a) => Pattern a IndexedType -> m (Label Kernel.Type)
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
    PTraitDictionary _ t trait@(Trait name _) ->
      pure (Label (translateType t) (dictVariable name trait))
    _ ->
      error "TODO"

dictVariable :: (Serializable t) => Name -> Trait t -> Name
dictVariable name trait = "$d_" <> name <> "__$instance_" <> serialize trait

translateClause :: (MonadReader KernelEnvironment m, Data a) => CompiledClause a IndexedType -> m (Kernel.Clause Kernel.Type KernelExpr)
translateClause =
  \case
    ECompiledClause (ll :| lls) e -> do
      e1 <- withLocalNames (labelName <$> lls) (translateExpression e)
      ll0 <- qualifyLabel ll
      pure (Kernel.Clause (translateLabel <$> (ll0 :| lls)) e1)

{-# INLINE qualifyLabel #-}
qualifyLabel :: (MonadReader KernelEnvironment m) => Label IndexedType -> m (Label IndexedType)
qualifyLabel (Label t name) = Label t <$> qualifyName name

{-# INLINE translateLabel #-}
translateLabel :: Label IndexedType -> Label Kernel.Type
translateLabel (Label t name) = Label (translateType t) name

translateBinaryOperator ::
  (MonadReader KernelEnvironment m, Data a) =>
  IndexedType ->
  IndexedType ->
  BinaryOperator ->
  NonEmpty (Expression a IndexedType) ->
  m KernelExpr
translateBinaryOperator t ot =
  \case
    OReverseComposition ->
      reverseCompositionOperator t
    OReverseApplication ->
      reverseApplicationOperator t
    OListConcatenation ->
      listConcatenationOperator t
    OLessThan ->
      binop Kernel.OLtInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OGreaterThan ->
      binop Kernel.OGtInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OLessThanOrEqual ->
      binop Kernel.OLteInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OGreaterThanOrEqual ->
      binop Kernel.OGteInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OLogicalAnd ->
      binop Kernel.OAnd (TIntrinsic IBool, TIntrinsic IBool)
    OLogicalOr ->
      binop Kernel.OOr (TIntrinsic IBool, TIntrinsic IBool)
    OAddition
      | TIntrinsic IInt32 == t ->
          binop Kernel.OAddInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OAddition
      | TIntrinsic IInt64 == t ->
          binop Kernel.OAddInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    OAddition
      | TIntrinsic IFloat == t ->
          binop Kernel.OAddFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    OAddition
      | TIntrinsic IDouble == t ->
          binop Kernel.OAddDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    OAddition ->
      error "TODO"
    OSubtraction
      | TIntrinsic IInt32 == t ->
          binop Kernel.OSubInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OSubtraction
      | TIntrinsic IInt64 == t ->
          binop Kernel.OSubInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    OSubtraction
      | TIntrinsic IFloat == t ->
          binop Kernel.OSubFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    OSubtraction
      | TIntrinsic IDouble == t ->
          binop Kernel.OSubDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    OSubtraction ->
      error "TODO"
    OMultiplication
      | TIntrinsic IInt32 == t ->
          binop Kernel.OMulInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    OMultiplication
      | TIntrinsic IInt64 == t ->
          binop Kernel.OMulInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    OMultiplication
      | TIntrinsic IFloat == t ->
          binop Kernel.OMulFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    OMultiplication
      | TIntrinsic IDouble == t ->
          binop Kernel.OMulDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    OMultiplication ->
      error "TODO"
    ODivision
      | TIntrinsic IInt32 == t ->
          binop Kernel.ODivInt32 (TIntrinsic IInt32, TIntrinsic IInt32)
    ODivision
      | TIntrinsic IInt64 == t ->
          binop Kernel.ODivInt64 (TIntrinsic IInt64, TIntrinsic IInt64)
    ODivision
      | TIntrinsic IFloat == t ->
          binop Kernel.ODivFloat (TIntrinsic IFloat, TIntrinsic IFloat)
    ODivision
      | TIntrinsic IDouble == t ->
          binop Kernel.ODivDouble (TIntrinsic IDouble, TIntrinsic IDouble)
    ODivision ->
      error "TODO"
    OStringConcatenation ->
      stringConcatenationOperator
    OEqualTo ->
      equalityOperator ot
    _ ->
      error "Not implemented"

equalityOperator :: (MonadReader KernelEnvironment m, Data a) => IndexedType -> NonEmpty (Expression a IndexedType) -> m KernelExpr
equalityOperator ot (e1 :| [e2]) = do
  o1 <- translateExpression e1
  o2 <- translateExpression e2
  case ot of
    (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqInt32 o1 o2))
    (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqInt64 o1 o2))
    (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqFloat o1 o2))
    (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqDouble o1 o2))
    (TIntrinsic IChar `TArrow` TIntrinsic IChar `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqChar o1 o2))
    (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) ->
      pure (Kernel.op (Kernel.OEqBool o1 o2))
    _ ->
      -- error "Not implemented"
      error (show ot)
equalityOperator _ _ = error "Not implemented"

stringConcatenationOperator :: (MonadReader KernelEnvironment m, Data a) => NonEmpty (Expression a IndexedType) -> m KernelExpr
stringConcatenationOperator es = do
  args <- traverse translateExpression es
  let t1 = translateType (TIntrinsic IString)
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (t1 `Kernel.arrow` t1 `Kernel.arrow` t1) "Core$.operator__string_concatenation"))
      args

listConcatenationOperator :: (MonadReader KernelEnvironment m, Data a) => IndexedType -> NonEmpty (Expression a IndexedType) -> m KernelExpr
listConcatenationOperator t es = do
  args <- traverse translateExpression es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (t1 `Kernel.arrow` t1 `Kernel.arrow` t1) "Core$.operator__list_concatenation"))
      args

reverseCompositionOperator :: (MonadReader KernelEnvironment m, Data a) => IndexedType -> NonEmpty (Expression a IndexedType) -> m KernelExpr
reverseCompositionOperator t es = do
  args <- traverse translateExpression es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (Kernel.foldType t1 (Kernel.typeOf <$> args)) "Core$.operator__reverse_composition"))
      args

reverseApplicationOperator :: (MonadReader KernelEnvironment m, Data a) => IndexedType -> NonEmpty (Expression a IndexedType) -> m KernelExpr
reverseApplicationOperator t es = do
  args <- traverse translateExpression es
  let t1 = translateType t
  pure $
    Kernel.app
      t1
      (Kernel.var (Label (Kernel.foldType t1 (Kernel.typeOf <$> args)) "Core$.operator__reverse_application"))
      args

binop :: (MonadReader KernelEnvironment m, Data a) => (KernelExpr -> KernelExpr -> Kernel.Op KernelExpr) -> (IndexedType, IndexedType) -> NonEmpty (Expression a IndexedType) -> m KernelExpr
binop op (t1, t2) (e1 :| [e2])
  | e1 `hasType` t1 && e2 `hasType` t2 = do
      o1 <- translateExpression e1
      o2 <- translateExpression e2
      pure (Kernel.op (op o1 o2))
binop _ _ _ = error "Implementation error"

{-# INLINE hasType #-}
hasType :: (Data a) => Expression a IndexedType -> IndexedType -> Bool
hasType e t = (typeOf e :: IndexedType) == t
