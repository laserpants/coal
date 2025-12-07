{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.Nats (passCompileNats) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module (ConstantDefinition (..), Definition (..), FunctionDefinition (..), Module (..))
import Control.Monad ((<=<))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Extras (Dictionary)

passCompileNats :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passCompileNats =
  Pass
    { passName = "CompileNats"
    , runPass = pass
    }

pass :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
pass = compileNats

class CompileNatsContext e where
  compileNats :: (Monad m) => e -> CompilerT a m e

instance (CompileNatsContext a) => CompileNatsContext [a] where
  compileNats = traverse compileNats

instance (CompileNatsContext a) => CompileNatsContext (NonEmpty a) where
  compileNats = traverse compileNats

instance (CompileNatsContext a) => CompileNatsContext (Dictionary a) where
  compileNats = traverse compileNats

convertConstructor :: (Monad m) => IndexedType -> CompilerT a m IndexedType
convertConstructor =
  \case
    TIntrinsic INat ->
      pure $ TConstructor KType "$Nat"
    TIntrinsic t ->
      pure (TIntrinsic t)
    TArrow t1 t2 ->
      TArrow <$> convertConstructor t1 <*> convertConstructor t2
    TApplication k t1 t2 ->
      TApplication k <$> convertConstructor t1 <*> convertConstructor t2
    TRow r ->
      TRow <$> traverse convertConstructor r
    TAlias name ts t ->
      TAlias name <$> traverse convertConstructor ts <*> convertConstructor t
    t ->
      pure t

instance (Monoid a, Data a) => CompileNatsContext (Expression a IndexedType) where
  compileNats = transformM (traverse convertConstructor <=< go)
   where
    go =
      \case
        EApplication a1 (TIntrinsic INat) (EConstructor _ (Label _ "Succ")) es ->
          pure $
            EApplication
              a1
              (TConstructor KType "$Nat")
              (EConstructor mempty (Label (TIntrinsic IInt32 `TArrow` TConstructor KType "$Nat") "$Succ"))
              ( EApplication
                  mempty
                  (TIntrinsic IInt32)
                  (EVariable mempty (Label (TConstructor KType "$Nat" `TArrow` TIntrinsic IInt32) "Builtin$.unpack_nat"))
                  es
                  :| []
              )
        EConstructor a (Label _ "Zero") ->
          pure (EConstructor a (Label (TConstructor KType "$Nat") "$Zero"))
        ECompiledMatch a t e cs ->
          ECompiledMatch a t e <$> traverse compileNats cs
        e ->
          pure e

instance (Monoid a) => CompileNatsContext (CompiledClause a IndexedType) where
  compileNats =
    \case
      ECompiledClause loc (Label _ "Succ" :| [Label _ s]) e -> do
        name <- supplied (freshName "nats")
        pure $
          ECompiledClause loc (Label (TConstructor KType "$Nat" `TArrow` TConstructor KType "$Nat") "$Succ" :| [Label (TIntrinsic IInt32) name]) $
            ERecursiveLet
              mempty
              (PVariable mempty (Label (TConstructor KType "$Nat") s))
              ( EIf
                  mempty
                  (TIntrinsic IInt32)
                  ( EApplication
                      mempty
                      (TIntrinsic IBool)
                      ( EBinaryOperator
                          mempty
                          (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                          OEqualTo
                      )
                      ( EVariable mempty (Label (TIntrinsic IInt32) name)
                          <| ELiteral mempty (LInt32 0)
                          :| []
                      )
                  )
                  (EConstructor mempty (Label (TConstructor KType "$Nat") "$Zero"))
                  ( EApplication
                      mempty
                      (TConstructor KType "$Nat")
                      (EConstructor mempty (Label (TIntrinsic IInt32 `TArrow` TConstructor KType "$Nat") "$Succ"))
                      ( EApplication
                          mempty
                          (TIntrinsic IInt32)
                          ( EBinaryOperator
                              mempty
                              (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                              OSubtraction
                          )
                          ( EVariable mempty (Label (TIntrinsic IInt32) name)
                              <| ELiteral mempty (LInt32 1)
                              :| []
                          )
                          :| []
                      )
                  )
              )
              e
      ECompiledClause loc (Label _ "Zero" :| []) e ->
        pure (ECompiledClause loc (Label (TConstructor KType "$Nat") "$Zero" :| []) e)
      c ->
        pure c

instance (Monoid a, Data a) => CompileNatsContext (Module a Kind IndexedType) where
  compileNats =
    \case
      Module p ns o ->
        Module p ns <$> compileNats o

instance (Monoid a, Data a) => CompileNatsContext (FunctionDefinition a IndexedType) where
  compileNats =
    \case
      FunctionDefinition a u w ps e ->
        FunctionDefinition a u w ps <$> compileNats e

instance (Monoid a, Data a) => CompileNatsContext (ConstantDefinition a IndexedType) where
  compileNats =
    \case
      ConstantDefinition a u w e ->
        ConstantDefinition a u w <$> compileNats e

instance (Monoid a, Data a) => CompileNatsContext (Definition a Kind IndexedType) where
  compileNats =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> compileNats f <*> traverse compileNats fs
      DConstant loc name g fs ->
        DConstant loc name <$> compileNats g <*> traverse compileNats fs
      o ->
        pure o
