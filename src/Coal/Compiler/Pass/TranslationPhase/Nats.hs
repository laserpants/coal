{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.Nats (passCompileNats) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import qualified Coal.Compiler.Builtin.Traits as Trait
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Kernel.Builtin.Objects (builtinInstance)
import Coal.Language
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Monad ((<=<))
import Control.Monad.Trans (lift)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Extras (Dictionary)

passCompileNats :: (Monad m) => Pass Metadata m (ProtoModule Metadata Kind IndexedType) (ProtoModule Metadata Kind IndexedType)
passCompileNats = Pass{runPass = bork}

bork :: (Monad m) => ProtoModule Metadata Kind IndexedType -> CompilerT a m (ProtoModule Metadata Kind IndexedType)
bork = compileNats

class CompileNatsContext e where
  compileNats :: (Monad m) => e -> CompilerT a m e

instance (CompileNatsContext a) => CompileNatsContext [a] where
  compileNats = traverse compileNats

instance (CompileNatsContext a) => CompileNatsContext (NonEmpty a) where
  compileNats = traverse compileNats

instance (CompileNatsContext a) => CompileNatsContext (Dictionary a) where
  compileNats = traverse compileNats

natType :: IndexedType
natType = TConstructor KType "$Nat"

convertConstructor :: (Monad m) => IndexedType -> CompilerT a m IndexedType
convertConstructor =
  \case
    TIntrinsic INat ->
      pure natType
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

instance (Monoid a, Data a) => CompileNatsContext (Expression a Kind IndexedType) where
  compileNats = transformM (traverse convertConstructor <=< go)
   where
    go =
      \case
        EApplication a1 (TIntrinsic INat) (EConstructor _ (Label _ "Succ")) es ->
          pure $
            EApplication
              a1
              natType
              (EConstructor mempty (Label (TIntrinsic IInt32 `TArrow` natType) "$Succ"))
              ( EApplication
                  mempty
                  (TIntrinsic IInt32)
                  (EVariable mempty (Label (natType `TArrow` TIntrinsic IInt32) "Builtin$.nat$_unpack"))
                  es
                  :| []
              )
        EConstructor a (Label _ "Zero") ->
          pure (EConstructor a (Label natType "$Zero"))
        ECompiledMatch a t e cs ->
          ECompiledMatch a t e <$> traverse compileNats cs
        e ->
          pure e

instance (Monoid a) => CompileNatsContext (CompiledClause a Kind IndexedType) where
  compileNats =
    \case
      ECompiledClause loc (Label _ "Succ" :| [Label _ s]) e -> do
        name <- supplied (freshName "nats")
        pure $
          ECompiledClause loc (Label (natType `TArrow` natType) "$Succ" :| [Label (TIntrinsic IInt32) name]) $
            ERecursiveLet
              mempty
              (PVariable mempty (Label natType s))
              ( EIf
                  mempty
                  (TIntrinsic IInt32)
                  ( EApplication
                      mempty
                      (TIntrinsic IBool)
                      (EVariable mempty (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool) (builtinInstance (Trait.comparable (TIntrinsic IInt32)) "(==)")))
                      ( EVariable mempty (Label (TIntrinsic IInt32) name)
                          <| ELiteral mempty (LInt32 0)
                          :| []
                      )
                  )
                  (EConstructor mempty (Label natType "$Zero"))
                  ( EApplication
                      mempty
                      natType
                      (EConstructor mempty (Label (TIntrinsic IInt32 `TArrow` natType) "$Succ"))
                      ( EApplication
                          mempty
                          (TIntrinsic IInt32)
                          (EVariable mempty (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(-)")))
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
        pure (ECompiledClause loc (Label natType "$Zero" :| []) e)
      c ->
        pure c

-- instance (Monoid a, Data a) => CompileNatsContext (Module a Kind IndexedType) where
--  compileNats =
--    \case
--      Module p ns o ->
--        Module p ns <$> compileNats o

instance (Monoid a, Data a) => CompileNatsContext (ProtoModule a Kind IndexedType) where
  compileNats =
    \case
      ProtoModule p ns o ->
        ProtoModule p ns <$> compileNats o

instance (Monoid a, Data a) => CompileNatsContext (ProtoLetDefinition a Kind IndexedType) where
  compileNats =
    \case
      ProtoLetDefinition{..} -> do
        newLetDefinitionExpression <- compileNats protoOletDefinitionExpression
        return
          ProtoLetDefinition
            { protoOletDefinitionExpression = newLetDefinitionExpression
            , ..
            }

-- instance (Monoid a, Data a) => CompileNatsContext (FunctionDefinition a IndexedType) where
--  compileNats =
--    undefined
----    \case
----      FunctionDefinition a u w ps e ->
----        FunctionDefinition a u w ps <$> compileNats e

instance (Monoid a, Data a) => CompileNatsContext (ProtoFunctionDefinition a Kind IndexedType) where
  compileNats =
    \case
      ProtoFunctionDefinition{..} -> do
        newLetDefinitionExpression <- compileNats protoOfunctionDefinitionExpression
        return
          ProtoFunctionDefinition
            { protoOfunctionDefinitionExpression = undefined
            , ..
            }

-- instance (Monoid a, Data a) => CompileNatsContext (ConstantDefinition a IndexedType) where
--  compileNats =
--    undefined
----         \case
----           ConstantDefinition a u w e ->
----             ConstantDefinition a u w <$> compileNats e

-- instance (Monoid a, Data a) => CompileNatsContext (Definition a Kind IndexedType) where
--  compileNats =
--    \case
--      DFunction loc name f fs ->
--        DFunction loc name <$> compileNats f <*> traverse compileNats fs
--      DConstant loc name g fs ->
--        DConstant loc name <$> compileNats g <*> traverse compileNats fs
--      o ->
--        pure o

instance (Monoid a, Data a) => CompileNatsContext (ProtoDefinition a Kind IndexedType) where
  compileNats =
    \case
      ProtoDLet loc name def ->
        ProtoDLet loc name <$> compileNats def
      ProtoDFunction loc name def ->
        ProtoDFunction loc name <$> compileNats def
      o ->
        pure o
