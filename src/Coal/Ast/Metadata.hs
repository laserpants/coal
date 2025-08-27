{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Ast.Metadata (Metadata (..), HasMetadata (..), metadataSpan) where

import Coal.Language.Expression
import Coal.Language.Pattern
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Coal.TypeSystem.Constraint.Generation.Internal (ConstraintsGenError (..), TypeAnnotationError (..))
import Data.Data (Data)
import Text.Megaparsec

data Metadata = Metadata
  { locationStart :: SourcePos
  , locationEnd :: SourcePos
  }
  deriving (Show, Eq, Ord, Read, Data)

--instance Show Metadata where
--  show _ = ""

defaultSourcePos :: SourcePos
defaultSourcePos =
  SourcePos
    { sourceName = "<unknown>"
    , sourceLine = mkPos 1
    , sourceColumn = mkPos 1
    }

instance Semigroup Metadata where
  lhs <> _ = lhs

instance Monoid Metadata where
  mempty = Metadata defaultSourcePos defaultSourcePos

class HasMetadata a where
  getMetadata :: a -> Metadata

instance HasMetadata (Expression Metadata t) where
  getMetadata =
    \case
      EAnnotation a _ _ -> a
      EApplication a _ _ _ -> a
      ELambda a _ _ -> a
      ELet a _ _ -> a
      ERecursiveLet a _ _ _ -> a
      EVariable a _ -> a
      EConstructor a _ -> a
      ELiteral a _ -> a
      EIf a _ _ _ _ -> a
      EUnaryOperator a _ _ -> a
      EBinaryOperator a _ _ -> a
      ERecord a _ _ _ -> a
      EListCons a _ _ _ -> a
      EListLiteral a _ _ -> a
      ETuple a _ _ -> a
      EMatch a _ _ _ -> a
      ECompiledMatch a _ _ _ -> a
      EFold a _ _ _ _ _ -> a
      EUnfold a _ _ _ _ _ -> a
      ESelect a _ _ -> a
      ECodataSelect a _ _ _ -> a
      ECodataFields a _ _ -> a
      ETraitDictionary a _ _ -> a
      EFocus{} -> error "Not implemented"

instance HasMetadata (Pattern Metadata t) where
  getMetadata =
    \case
      PAnnotation a _ _ -> a
      PAny a _ -> a
      PVariable a _ -> a
      PConstructor a _ _ -> a
      PLiteral a _ -> a
      PRecord a _ _ _ -> a
      PListCons a _ _ _ -> a
      PListLiteral a _ _ -> a
      PTuple a _ _ -> a
      POr a _ _ _ -> a
      PAs a _ _ -> a
      PShorthand a _ -> a
      PAtVariable a _ _ -> a
      PTraitDictionary a _ _ -> a

instance HasMetadata (InferenceRule k Metadata) where
  getMetadata =
    \case
      InferenceRulePlaceholder -> Metadata defaultSourcePos defaultSourcePos -- TODO error "Not implemented"
      RuleAnnotation a _ _ -> a
      RuleApplication a _ _ -> a
      RuleIfCondition a _ -> a
      RuleIfBranches a _ _ -> a
      RuleLetBindingPattern a _ _ -> a
      RuleLetImplicit a _ _ _ -> a
      RuleMatchClauseGuard a -> a
      RuleMatchClauseExpressions a -> a
      RuleMatchClausePatterns a -> a
      RuleBinaryOperator a -> a
      RuleTopLevelFunction a -> a
      RuleTopLevelConstant a -> a
      RuleTypeConstraint a _ _ _ -> a
      RuleDataConstructor a _ _ _ -> a

instance HasMetadata (ConstraintsGenError Metadata) where
  getMetadata =
    \case
      ENoDataConstructor a _ -> a
      ENoCodataAccessor a _ -> a
      EDataConstructorArityMismatch a _ _ _ -> a
      EIllFormedTypeAnnotation err -> getMetadata err

instance HasMetadata (TypeAnnotationError Metadata) where
  getMetadata =
    \case
      EAnnotationKindMismatch a -> a
      EAnnotationConstructor a _ -> a
      EAnnotationMonomorphicType a _ _ -> a
      EAnnotationNonDistinctParameters _ -> error "TODO"

instance HasMetadata (Assumption Metadata t) where
  getMetadata =
    \case
      Assumption a _ _ ->
        a

metadataSpan :: (HasMetadata a) => a -> a -> Metadata
metadataSpan lhs rhs = Metadata (locationStart (getMetadata lhs)) (locationEnd (getMetadata rhs))
