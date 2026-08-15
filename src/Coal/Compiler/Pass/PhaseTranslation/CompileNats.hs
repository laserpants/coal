{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhaseTranslation.CompileNats

Compile natural number types and constructors into efficient runtime representation.

This pass transforms the @nat@ type and its constructors (@Zero@ and @Succ@) into
an internal representation backed by @int32@ for efficient execution. The
transformation converts the intrinsic @nat@ type to a compiled representation
that relies on primitive integer operations.

The @nat@ type constructors are transformed as follows:

@
Zero => $Zero
Succ(n) => $Succ(unpack(n))
@

Where unpacking converts the @nat@ to its @int32@ backing value. Pattern matching
on @nat@ constructors is also compiled to efficient integer comparisons:

@
match(n) {
  | Zero => ...
  | Succ(m) => ...  // m is reconstructed from int32
}
@

becomes runtime checks on the @int32@ value, reconstructing the recursive @nat@
structure only when needed. This provides an efficient implementation of
natural numbers while maintaining the structural recursion guarantees.
-}
module Coal.Compiler.Pass.PhaseTranslation.CompileNats (
  passCompileNats,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import qualified Coal.Compiler.Builtin.Traits as Trait
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Control.Monad ((<=<))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Extras (Dictionary, Name)

{- | Natural number compilation pass.

Transform @nat@ types and constructors into an efficient @int32@-backed runtime
representation. Convert @nat@ pattern matching into integer comparisons and
reconstruct recursive @nat@ structures only when needed, providing efficient
natural number operations while preserving structural recursion semantics.
-}
passCompileNats :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passCompileNats = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT a m (Module Metadata Kind IndexedType)
passImpl = compileNats

builtinInstance :: (Serializable t) => Trait t -> Name -> Name
builtinInstance trait name = instanceLabel trait ("Builtin$." <> name)

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

instance CompileNatsContext IndexedType where
  compileNats =
    \case
      TIntrinsic INat ->
        pure natType
      TIntrinsic t ->
        pure (TIntrinsic t)
      TArrow t1 t2 ->
        TArrow <$> compileNats t1 <*> compileNats t2
      TApplication k t1 t2 ->
        TApplication k <$> compileNats t1 <*> compileNats t2
      TRow r ->
        TRow <$> traverse compileNats r
      TAlias name ts t ->
        TAlias name <$> traverse compileNats ts <*> compileNats t
      t ->
        pure t

instance (Monoid a, Data a) => CompileNatsContext (Expression a Kind IndexedType) where
  compileNats = transformM (traverse compileNats <=< go)
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

instance (Monoid a, Data a) => CompileNatsContext (Module a Kind IndexedType) where
  compileNats =
    \case
      Module p ns o ->
        Module p ns <$> compileNats o

instance (Monoid a, Data a) => CompileNatsContext (LetDefinition a Kind IndexedType) where
  compileNats =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- compileNats letDefinitionExpression
        return
          LetDefinition
            { letDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance (Monoid a, Data a) => CompileNatsContext (FunctionDefinition a Kind IndexedType) where
  compileNats =
    \case
      FunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- compileNats functionDefinitionExpression
        return
          FunctionDefinition
            { functionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

instance (Monoid a, Data a) => CompileNatsContext (Definition a Kind IndexedType) where
  compileNats =
    \case
      DLet loc name def ->
        DLet loc name <$> compileNats def
      DFunction loc name def ->
        DFunction loc name <$> compileNats def
      o ->
        pure o
