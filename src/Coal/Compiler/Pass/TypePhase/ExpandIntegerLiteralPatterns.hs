{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpandIntegerLiteralPatterns (passExpandIntegerLiteralPatterns) where

import Coal.AST.Metadata (Metadata (..))
import Coal.AST.Shorthand
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module (Module (..), principalPath)
import Coal.Language.Module.Definition (Definition (..))
import Coal.Language.Module.Definition.Constant (ConstantDefinition (..))
import Coal.Language.Module.Definition.Fold (FoldDefinition (..))
import Coal.Language.Module.Definition.Function (FunctionDefinition (..))
import Coal.Language.Module.Definition.Unfold (UnfoldDefinition (..))
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import Control.Monad.Writer
import qualified Data.ByteString.Char8 as ByteString
import Data.Generics.Uniplate.Data (descendM, transformM)
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Int (Int32, Int64)
import TextShow (showt)

passExpandIntegerLiteralPatterns :: (Monad m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind ())
passExpandIntegerLiteralPatterns = Pass{runPass = expandIntegerLiteralPatterns}

class TransformContext e where
  expandIntegerLiteralPatterns :: (Monad m) => e -> CompilerT Metadata m e

instance (TransformContext e) => TransformContext [e] where
  expandIntegerLiteralPatterns = traverse expandIntegerLiteralPatterns

instance (TransformContext e) => TransformContext (NonEmpty e) where
  expandIntegerLiteralPatterns = traverse expandIntegerLiteralPatterns

instance TransformContext (Expression Metadata ()) where
  expandIntegerLiteralPatterns =
    \case
      EMatch a t e cs ->
        EMatch a t e . NonEmpty.fromList <$> traverse (expandClause a e) pairs
       where
        pairs = [(p, ps) | (p : ps) <- tails (NonEmpty.toList cs)]
      e ->
        descendM expandIntegerLiteralPatterns e

expandClause :: (Monad m) => Metadata -> Expression Metadata () -> (Clause Metadata (), [Clause Metadata ()]) -> CompilerT Metadata m (Clause Metadata ())
expandClause _ expr (cl@(EClause a p (CPlain a1 gs e1 :| [])), ds) = do
  (q, ints) <- runWriterT (transformM collectIntegerLiteralPatterns p)
  case (ints, ds) of
    ([], _) ->
      pure cl
    (_, []) -> do
      path <- gets compilerCurrentModule
      tellErrors [NonExhaustivePatterns (ErrorLocation (principalPath path) a)]
      throwError PatternAnomaly
    (_, c : cs) -> do
      e2 <-
        expandIntegerLiteralPatterns $
          ifE
            (foldr numericLiteral (literalBoolE True) ints)
            e1
            (matchE expr (c :| cs))
      pure (EClause a q (CPlain a1 gs e2 :| []))
expandClause _ _ _ = error "Implementation error"

numericLiteral :: (Label (), Integer) -> Expression Metadata () -> Expression Metadata ()
numericLiteral (ll, int) e1 =
  applicationE
    opAndE
    ( e1
        :| [ applicationE
              opEqualToE
              (EVariable mempty ll :| [fromLiteral int])
           ]
    )

fromLiteral :: Integer -> Expression Metadata ()
fromLiteral int
  | int <= fromIntegral (maxBound :: Int32) =
      applicationE (varE "from_int32") (ELiteral mempty (LInt32 (fromIntegral int)) :| [])
  | int <= fromIntegral (maxBound :: Int64) =
      applicationE (varE "from_int64") (ELiteral mempty (LInt64 (fromIntegral int)) :| [])
  | otherwise =
      applicationE (varE "from_literal") (ELiteral mempty (LString (ByteString.pack $ show int)) :| [])

collectIntegerLiteralPatterns :: (Monad m) => Pattern Metadata () -> WriterT [(Label (), Integer)] (CompilerT Metadata m) (Pattern Metadata ())
collectIntegerLiteralPatterns =
  \case
    PInteger a t int -> do
      n <- lift (supplied id)
      let ll = Label t ("int" <> ".[" <> showt n <> "]")
      tell [(ll, int)]
      pure (PVariable a ll)
    p ->
      pure p

instance TransformContext (FunctionDefinition Metadata ()) where
  expandIntegerLiteralPatterns =
    \case
      FunctionDefinition a u w ps e ->
        FunctionDefinition a u w ps <$> expandIntegerLiteralPatterns e

instance TransformContext (ConstantDefinition Metadata ()) where
  expandIntegerLiteralPatterns =
    \case
      ConstantDefinition a u w e ->
        ConstantDefinition a u w <$> expandIntegerLiteralPatterns e

instance TransformContext (Definition Metadata Kind ()) where
  expandIntegerLiteralPatterns =
    \case
      DConstant loc name g fs ->
        DConstant loc name
          <$> expandIntegerLiteralPatterns g
          <*> traverse expandIntegerLiteralPatterns fs
      DFunction loc name f fs ->
        DFunction loc name
          <$> expandIntegerLiteralPatterns f
          <*> traverse expandIntegerLiteralPatterns fs
      DFold loc n (FoldDefinition with cs e) ->
        DFold loc n . FoldDefinition with cs <$> traverse expandIntegerLiteralPatterns e
      DUnfold loc n (UnfoldDefinition with ps d me) ->
        DUnfold loc n . UnfoldDefinition with ps d <$> traverse expandIntegerLiteralPatterns me
      d ->
        pure d

instance TransformContext (Module Metadata Kind ()) where
  expandIntegerLiteralPatterns =
    \case
      Module p ns o -> do
        setCompilerCurrentModuleC p
        Module p ns <$> traverse expandIntegerLiteralPatterns o
