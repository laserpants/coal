{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Transform.Nats (
  NatExpansion (..),
  CompileNatsContext (..),
  evalNatExpansion,
  runNatExpansion,
)
where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (suppliedName)
import Coal.Language
import Coal.Language.Module (ConstantDef (..), Definition (..), FunctionDef (..), Module (..))
import Control.Monad ((<=<))
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Extra (Dictionary, Name)

newtype NatExpansion a = NatExpansion {natExpansionStack :: RWS Name () Int a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Name
    , MonadState Int
    )

evalNatExpansion :: Name -> Int -> NatExpansion a -> a
evalNatExpansion r s = fst . runNatExpansion r s

runNatExpansion :: Name -> Int -> NatExpansion a -> (a, Int)
runNatExpansion r s e = (a, s')
 where
  (a, s', _) = runRWS (natExpansionStack e) r s

class CompileNatsContext a where
  compileNats :: a -> NatExpansion a

instance (CompileNatsContext a) => CompileNatsContext [a] where
  compileNats = traverse compileNats

instance (CompileNatsContext a) => CompileNatsContext (NonEmpty a) where
  compileNats = traverse compileNats

instance (CompileNatsContext a) => CompileNatsContext (Dictionary a) where
  compileNats = traverse compileNats

convertConstructor :: IndexedType -> NatExpansion IndexedType
convertConstructor =
  \case
    TIntrinsic INat ->
      pure $ TConstructor KType "$Nat"
    TIntrinsic t ->
      TIntrinsic <$> traverse convertConstructor t
    TArrow t1 t2 ->
      TArrow <$> convertConstructor t1 <*> convertConstructor t2
    TApplication k t ts ->
      TApplication k <$> convertConstructor t <*> traverse convertConstructor ts
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
                  (EVariable mempty (Label (TConstructor KType "$Nat" `TArrow` TIntrinsic IInt32) "Core$.unpack_nat"))
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
      ECompiledClause (Label _ "Succ" :| [Label _ s]) e -> do
        name <- suppliedName
        pure $
          ECompiledClause (Label (TConstructor KType "$Nat" `TArrow` TConstructor KType "$Nat") "$Succ" :| [Label (TIntrinsic IInt32) name]) $
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
      ECompiledClause (Label _ "Zero" :| []) e ->
        pure (ECompiledClause (Label (TConstructor KType "$Nat") "$Zero" :| []) e)
      c ->
        pure c

instance (Monoid a, Data a) => CompileNatsContext (Module a Kind IndexedType) where
  compileNats =
    \case
      Module p ns o ->
        Module p ns <$> compileNats o

instance (Monoid a, Data a) => CompileNatsContext (FunctionDef a IndexedType) where
  compileNats =
    \case
      FunctionDef a u w ps e ->
        FunctionDef a u w ps <$> compileNats e

instance (Monoid a, Data a) => CompileNatsContext (ConstantDef a IndexedType) where
  compileNats =
    \case
      ConstantDef a u w e ->
        ConstantDef a u w <$> compileNats e

instance (Monoid a, Data a) => CompileNatsContext (Definition a Kind IndexedType) where
  compileNats =
    \case
      DFunction loc name f fs ->
        DFunction loc name <$> compileNats f <*> traverse compileNats fs
      DConstant loc name g fs ->
        DConstant loc name <$> compileNats g <*> traverse compileNats fs
      -- TODO
      o ->
        pure o
