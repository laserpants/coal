module Coal.Kernel.Pipeline.Invariant (
  InvariantError (..),
  checkCaseExpressionsCanonical,
  checkConstructorsSaturated,
  checkLocalNamesUnique,
  checkLambdasFlattened,
  checkLambdasLifted,
  checkTopLevelFunctionsNormalized,
  checkLogicalOperatorsTranslated,
  checkFunctionResultsSaturated,
  checkLetBindingsSimplified,
  checkAdministrativeNormalForm,
) where

import Coal.Kernel.Pipeline.Invariant.AdministrativeNormalForm (checkAdministrativeNormalForm)
import Coal.Kernel.Pipeline.Invariant.CaseExpressionsCanonical (checkCaseExpressionsCanonical)
import Coal.Kernel.Pipeline.Invariant.ConstructorsSaturated (checkConstructorsSaturated)
import Coal.Kernel.Pipeline.Invariant.Error (InvariantError (..))
import Coal.Kernel.Pipeline.Invariant.FunctionResultsSaturated (checkFunctionResultsSaturated)
import Coal.Kernel.Pipeline.Invariant.LambdasFlattened (checkLambdasFlattened)
import Coal.Kernel.Pipeline.Invariant.LambdasLifted (checkLambdasLifted)
import Coal.Kernel.Pipeline.Invariant.LetBindingsSimplified (checkLetBindingsSimplified)
import Coal.Kernel.Pipeline.Invariant.LocalNamesUnique (checkLocalNamesUnique)
import Coal.Kernel.Pipeline.Invariant.LogicalOperatorsTranslated (checkLogicalOperatorsTranslated)
import Coal.Kernel.Pipeline.Invariant.TopLevelFunctionsNormalized (checkTopLevelFunctionsNormalized)
