module Coal.Compiler.TypeInference.Errors where

import Coal.Ast.Metadata
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Data.Text (Text)

prettyTypeInferenceRuleViolation :: InferenceRule k Metadata -> Text
prettyTypeInferenceRuleViolation = undefined
