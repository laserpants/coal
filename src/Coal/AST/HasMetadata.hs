-- +
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Coal.AST.HasMetadata (HasMetadata (..), metadataSpan) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Language.Expression (Expression (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.ProtoLanguage.ProtoDefinition (ProtoDefinition (..))
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Coal.TypeSystem.Constraint.Generation.Stack (ConstraintsGenError (..), TypeAnnotationError (..))

class HasMetadata a where
  getMetadata :: a -> Metadata

instance HasMetadata Metadata where
  getMetadata = id

instance HasMetadata (Expression Metadata () t) where
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
      EOperator a _ _ -> a
      ERecord a _ _ _ -> a
      EListCons a _ _ _ -> a
      EListLiteral a _ _ -> a
      ETuple a _ _ -> a
      EMatch a _ _ _ -> a
      ECompiledMatch a _ _ _ -> a
      EFold a _ _ _ -> a
      ESelect a _ _ -> a
      ETraitInstance a _ _ -> a
      ELambdaMatch a _ _ -> a
      EFFICall a _ _ _ _ -> a
      EDoBlock a _ -> a
      EFocus a _ _ _ _ _ -> a

instance HasMetadata (Pattern Metadata () t) where
  getMetadata =
    \case
      PAnnotation a _ _ -> a
      PAny a _ -> a
      PVariable a _ -> a
      PConstructor a _ _ -> a
      PLiteral a _ -> a
      PInteger a _ _ -> a
      PRecord a _ _ _ -> a
      PListCons a _ _ _ -> a
      PListLiteral a _ _ -> a
      PTuple a _ _ -> a
      POr a _ _ _ -> a
      PAs a _ _ -> a
      PShorthand a _ -> a
      PAtVariable a _ -> a
      PNamedFold a _ _ -> a
      PTraitInstance a _ _ -> a

instance HasMetadata (InferenceRule k Metadata) where
  getMetadata =
    \case
      RuleAnnotation a _ _ -> a
      RuleApplication a _ _ -> a
      RuleIfCondition a _ -> a
      RuleIfBranches a _ _ -> a
      RuleLetBindingPattern a _ _ -> a
      RuleLetImplicit a _ _ _ -> a
      RuleMatchClauseGuard a -> a
      RuleMatchClauseExpressions a -> a
      RuleMatchClausePatterns a -> a
      RuleOperator a -> a
      RuleTopLevelFunction a -> a
      RuleTopLevelConstant a -> a
      RuleTypeConstraint a _ _ _ -> a
      RuleDataConstructor a _ _ _ -> a
      RuleEntrypoint a _ -> a
      RuleTuple a _ _ -> a
      RuleListLiteral a _ -> a
      RuleListConstructor a _ _ -> a
      RuleSelectEquality a _ _ -> a
      RuleRecordEquality a _ _ -> a
      RuleAssumption a _ _ -> a
      RuleAsConstraint a -> a
      RuleRecordField a _ _ -> a
      RuleRecordLacks a _ _ -> a
      RuleTailRow a _ _ -> a
      RuleFoldType a -> a
      RuleOrConstraint a _ _ -> a
      RuleTraitInstance a _ _ -> a
      RuleAssumptionExplicit a _ _ -> a

instance HasMetadata (ConstraintsGenError Metadata) where
  getMetadata =
    \case
      ENoDataConstructor a _ -> a
      EDataConstructorArityMismatch a _ _ _ -> a
      EIllFormedTypeAnnotation err -> getMetadata err
      EFoldPatternInRegularMatch a -> a

instance HasMetadata (TypeAnnotationError Metadata) where
  getMetadata =
    \case
      EAnnotationKindMismatch a -> a
      EAnnotationConstructor a _ -> a
      EAnnotationMonomorphicType a _ _ -> a
      EAnnotationNonDistinctParameter a _ -> a

instance HasMetadata (Assumption Metadata t) where
  getMetadata =
    \case
      Assumption a _ _ -> a

instance HasMetadata (ProtoDefinition Metadata k ()) where
  getMetadata =
    \case
      ProtoDType a _ _ -> a
      ProtoDTypeAlias a _ _ -> a
      ProtoDFunction a _ _ -> a
      ProtoDFunctionGroup a _ _ -> a
      ProtoDFold a _ _ -> a
      ProtoDLet a _ _ -> a
      ProtoDImport a _ _ -> a
      ProtoDNamespaceImport a _ -> a
      ProtoDTrait a _ _ -> a
      ProtoDInstance a _ -> a

metadataSpan :: (HasMetadata a) => a -> a -> Metadata
metadataSpan lhs rhs =
  Metadata
    (locationStart (getMetadata lhs))
    (locationEnd (getMetadata rhs))
