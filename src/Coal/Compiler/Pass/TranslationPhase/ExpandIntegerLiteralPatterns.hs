{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.ExpandIntegerLiteralPatterns (passExpandIntegerLiteralPatterns) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Control.Monad.Writer
import qualified Data.ByteString.Char8 as ByteString
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM, transformM)
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Int (Int32, Int64)
import TextShow (showt)

passExpandIntegerLiteralPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passExpandIntegerLiteralPatterns = Pass{runPass = bork}

bork :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
bork = expandIntegerLiteralPatterns

class TransformContext e where
  expandIntegerLiteralPatterns :: (Monad m) => e -> CompilerT Metadata m e

instance (TransformContext e) => TransformContext [e] where
  expandIntegerLiteralPatterns = traverse expandIntegerLiteralPatterns

instance (TransformContext e) => TransformContext (NonEmpty e) where
  expandIntegerLiteralPatterns = traverse expandIntegerLiteralPatterns

instance (Data k) => TransformContext (Expression Metadata k IndexedType) where
  expandIntegerLiteralPatterns =
    \case
      EMatch a t e cs ->
        EMatch a t e . NonEmpty.fromList <$> traverse (expandClause a e) pairs
       where
        pairs = [(p, ps) | (p : ps) <- tails (NonEmpty.toList cs)]
      e ->
        descendM expandIntegerLiteralPatterns e

expandClause :: (Monad m, Data k) => Metadata -> Expression Metadata k IndexedType -> (Clause Metadata k IndexedType, [Clause Metadata k IndexedType]) -> CompilerT Metadata m (Clause Metadata k IndexedType)
expandClause _ expr (EClause a p (CPlain a1 gs e1 :| []), ds) = do
  e1' <- expandIntegerLiteralPatterns e1
  (q, ints) <- runWriterT (transformM collectIntegerLiteralPatterns p)
  case (ints, ds) of
    ([], _) ->
      pure (EClause a p (CPlain a1 gs e1' :| []))
    (_, []) -> do
      -- path <- gets compilerCurrentModule
      path <- gets compilerCurrentPath
      tellErrors [NonExhaustivePatterns (ErrorLocation (principalPath path) a)]
      throwError PatternAnomaly
    (_, c : cs) -> do
      e2 <-
        expandIntegerLiteralPatterns $
          EIf
            mempty
            (typeOf e1')
            (foldr numericLiteral (ELiteral mempty (LBool True)) ints)
            e1'
            (EMatch mempty (typeOf e1') expr (c :| cs))
      pure (EClause a q (CPlain a1 gs e2 :| []))
expandClause _ _ _ = error "Implementation error"

numericLiteral :: (Label IndexedType, Integer) -> Expression Metadata k IndexedType -> Expression Metadata k IndexedType
numericLiteral (ll@(Label t _), int) e1 =
  EApplication
    mempty
    (TIntrinsic IBool)
    (EOperator mempty (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
    ( e1
        :| [ EApplication
              mempty
              (TIntrinsic IBool)
              (EVariable mempty (Label (t `TArrow` t `TArrow` TIntrinsic IBool) "(==)"))
              (EVariable mempty ll :| [fromLiteral t int])
           ]
    )

fromLiteral :: IndexedType -> Integer -> Expression Metadata k IndexedType
fromLiteral t int
  | int <= fromIntegral (maxBound :: Int32) =
      EApplication mempty t (EVariable mempty (Label (TIntrinsic IInt32 `TArrow` t) "from_int32")) (ELiteral mempty (LInt32 (fromIntegral int)) :| [])
  | int <= fromIntegral (maxBound :: Int64) =
      EApplication mempty t (EVariable mempty (Label (TIntrinsic IInt64 `TArrow` t) "from_int64")) (ELiteral mempty (LInt64 (fromIntegral int)) :| [])
  | otherwise =
      EApplication
        mempty
        t
        (EVariable mempty (Label (TIntrinsic IString `TArrow` t) "from_bignum"))
        ( EApplication
            mempty
            (TIntrinsic IBignum)
            (EVariable mempty (Label (TIntrinsic IString `TArrow` t) "number$_unsafe_parse_bignum"))
            (ELiteral mempty (LString (ByteString.pack $ show int)) :| [])
            :| []
        )

collectIntegerLiteralPatterns :: (Monad m) => Pattern Metadata k IndexedType -> WriterT [(Label IndexedType, Integer)] (CompilerT Metadata m) (Pattern Metadata k IndexedType)
collectIntegerLiteralPatterns =
  \case
    PInteger a t int -> do
      n <- lift (supplied id)
      let ll = Label t ("int" <> ".[" <> showt n <> "]")
      tell [(ll, int)]
      pure (PVariable a ll)
    p ->
      pure p

instance TransformContext (FunctionDefinition Metadata Kind IndexedType) where
  expandIntegerLiteralPatterns =
    \case
      FunctionDefinition{..} -> do
        newFunctiongDefinitionExpression <- expandIntegerLiteralPatterns functionDefinitionExpression
        return $
          FunctionDefinition
            { functionDefinitionExpression = newFunctiongDefinitionExpression
            , ..
            }

instance TransformContext (LetDefinition Metadata Kind IndexedType) where
  expandIntegerLiteralPatterns =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- expandIntegerLiteralPatterns letDefinitionExpression
        return $
          LetDefinition
            { letDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance TransformContext (Definition Metadata Kind IndexedType) where
  expandIntegerLiteralPatterns =
    \case
      DFunction a name def ->
        DFunction a name <$> expandIntegerLiteralPatterns def
      DLet a name def ->
        DLet a name <$> expandIntegerLiteralPatterns def
      d ->
        pure d

instance TransformContext (Module Metadata Kind IndexedType) where
  expandIntegerLiteralPatterns =
    \case
      Module{..} -> do
        setCurrentPathC modulePath
        -- setCompilerCurrentModuleC modulePath
        setCurrentPathC modulePath
        newModuleDefinitions <- traverse expandIntegerLiteralPatterns moduleDefinitions
        return $
          Module
            { moduleDefinitions = newModuleDefinitions
            , ..
            }
