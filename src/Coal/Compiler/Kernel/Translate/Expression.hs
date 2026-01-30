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
import Coal.Compiler.Kernel.Translate.Record (extractRow, translateRecord)
import Coal.Compiler.Kernel.Translate.Type (translateType)
import Coal.Compiler.Stack (CompilerT)
import Coal.Kernel.Compiler (KernelExpr)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Text as Text

translateExpression :: (Monad m, Data a) => Expression a () IndexedType -> CompilerT a m KernelExpr
translateExpression =
  \case
    EAnnotation _ _ e ->
      translateExpression e
    EApplication _ t (EOperator _ _ op) es ->
      translateOperator translateExpression t op es
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
    EVariable loc (Label t name)
      -- Top-level fold
      | "!" `Text.isPrefixOf` name ->
          translateExpression (EVariable loc (Label t (Text.drop 1 name)))
    EVariable _ (Label t name) ->
      Kernel.var . Label (translateType t) <$> qualifyName name
    EConstructor _ (Label t name) ->
      Kernel.var . Label (translateType t) <$> qualifyName name
    ELiteral _ p ->
      pure (Kernel.lit (translatePrimitive p))
    EIf _ _ e1 e2 e3 ->
      Kernel.if_
        <$> translateExpression e1
        <*> translateExpression e2
        <*> translateExpression e3
    ERecord _ t d me -> do
      translateRecord translateExpression t d me
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
              (Label (Kernel.arrow t1 (Kernel.record r)) "$Record" :| [Label t1 "$row"])
              ( Kernel.sel
                  (Kernel.Focus field (translateLabel ll) (Label (Kernel.dropField field r) "_"))
                  (Kernel.var (Label r "$row"))
                  (Kernel.var (translateLabel ll))
              )
              :| []
          )
    EFocus _ name0 ll1 ll2@(Label t1 _) e1 e2 -> do
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
    ETraitInstance _ t trait ->
      pure (Kernel.var (Label (translateType t) (dictionaryLabel trait)))
    EFFICall _ _ (Label t name) es e ->
      Kernel.call (Label (translateType t) name)
        <$> traverse translateExpression es
        <*> translateExpression e
    EMatch{} ->
      error "Implementation error"
    ELambdaMatch{} ->
      error "Implementation error"
    _ ->
      error "Not implemented"

translateBinding :: (Monad m, Data a) => Binding Expression a () IndexedType -> CompilerT a m (Kernel.Binding Kernel.Type KernelExpr)
translateBinding =
  \case
    BPattern _ (PVariable _ ll) e -> do
      e1 <- withLocalNames [name | name <- [labelName ll], Text.isPrefixOf "$fold" name] (translateExpression e)
      pure (Kernel.Binding (translateLabel ll) e1)
    _ ->
      error "Not implemented"

translateClause :: (Monad m, Data a) => CompiledClause a () IndexedType -> CompilerT a m (Kernel.Clause Kernel.Type KernelExpr)
translateClause =
  \case
    ECompiledClause _ (ll :| lls) e -> do
      e1 <- withLocalNames (labelName <$> lls) (translateExpression e)
      ll0 <- qualifyLabel ll
      pure (Kernel.Clause (translateLabel <$> (ll0 :| lls)) e1)

{-# INLINE qualifyLabel #-}
qualifyLabel :: (Monad m) => Label IndexedType -> CompilerT a m (Label IndexedType)
qualifyLabel (Label t name) = Label t <$> qualifyName name

{-# INLINE translateLabel #-}
translateLabel :: Label IndexedType -> Label Kernel.Type
translateLabel (Label t name) = Label (translateType t) name
