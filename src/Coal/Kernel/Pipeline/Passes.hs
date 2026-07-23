module Coal.Kernel.Pipeline.Passes (
  -- * Entire pipeline
  pipeline,

  -- * Sub-pipelines
  structuralNorm,
  functionalNorm,
  controlFlowNorm,
) where

import Control.Monad ((>=>))

import Coal.Kernel.Language.Module (Module)
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Pipeline (Pass)
import Coal.Kernel.Pipeline.Pass.AdministrativeNormalForm (administrativeNormalForm)
import Coal.Kernel.Pipeline.Pass.CaseExpressionCanonicalization (
  caseExpressionCanonicalization,
 )
import Coal.Kernel.Pipeline.Pass.ConstructorSaturation (constructorSaturation)
import Coal.Kernel.Pipeline.Pass.FunctionResultsSaturation (functionResultsSaturation)
import Coal.Kernel.Pipeline.Pass.LambdaFlattening (lambdaFlattening)
import Coal.Kernel.Pipeline.Pass.LambdaLifting (lambdaLifting)
import Coal.Kernel.Pipeline.Pass.LetBindingSimplification (letBindingSimplification)
import Coal.Kernel.Pipeline.Pass.LocalNameCanonicalization (localNameCanonicalization)
import Coal.Kernel.Pipeline.Pass.LogicalOperatorTranslation (logicalOperatorTranslation)
import Coal.Kernel.Pipeline.Pass.TopLevelFunctionNormalization (topLevelFunctionNormalization)

{- | Passes 1–4: Structural normalization.

1. 'caseExpressionCanonicalization' – sort case clauses lexicographically by constructor name.
2. 'localNameCanonicalization'      – alpha-rename every locally-bound name @x@ to a unique @x.n@.
3. 'lambdaFlattening'               – collapse nested @fn(a) => fn(b) => e@ into @fn(a, b) => e@.
4. 'constructorSaturation'          – eta-expand partial constructor applications.
-}
structuralNorm :: (Monad m) => Pass m (Module Type) (Module Type)
structuralNorm =
  caseExpressionCanonicalization
    >=> localNameCanonicalization
    >=> lambdaFlattening
    >=> constructorSaturation

{- | Passes 5–7: Functional normalization

5. 'lambdaLifting'                 – lift lambda expressions to top-level definitions.
6. 'topLevelFunctionNormalization' – merge function-body lambdas; promote constant lambdas.
7. 'functionResultsSaturation'     – eta-expand functions whose result type is a function type.
-}
functionalNorm :: (Monad m) => Pass m (Module Type) (Module Type)
functionalNorm =
  lambdaLifting
    >=> topLevelFunctionNormalization
    >=> functionResultsSaturation

{- | Passes 8–10: Control-flow normalization

8.  'logicalOperatorTranslation' – desugar @&&@ / @||@ into @if@ expressions.
9.  'letBindingSimplification'   – eliminate pure-alias @let x = y@ bindings.
10. 'administrativeNormalForm'   – extract every non-atomic sub-expression into a @let@.
-}
controlFlowNorm :: (Monad m) => Pass m (Module Type) (Module Type)
controlFlowNorm =
  logicalOperatorTranslation
    >=> letBindingSimplification
    >=> administrativeNormalForm

-- | The full normalization pipeline (all passes).
pipeline :: (Monad m) => Pass m (Module Type) (Module Type)
pipeline = structuralNorm >=> functionalNorm >=> controlFlowNorm
