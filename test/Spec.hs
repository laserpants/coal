import qualified Noll.Language.Type.IndexedSpec

-- import qualified Noll.TypeSystem.ConstraintSolverSpec
-- import qualified Noll.TypeSystem.KindConstraint.CollectSpec
-- import qualified Noll.TypeSystem.KindSubstitutionSpec
-- import qualified Noll.TypeSystem.TypeConstraint.CollectSpec
-- import qualified Noll.TypeSystem.TypeSubstitutionSpec

import qualified Lang.FreeVarsSpec
import qualified Lang.UtilsSpec
import qualified Noll.Compiler.NormalizeObjectsSpec
import qualified Noll.Compiler.PatternMatchingExamples.Test01
import qualified Noll.Compiler.PatternMatchingExamples.Test02
import qualified Noll.Compiler.PatternMatchingSpec
import qualified Noll.Compiler.Transform.FoldSpec
import qualified Noll.Compiler.Transform.Pattern.DesugarSpec
import qualified Noll.Compiler.Transform.Pattern.OrExpansionSpec
import qualified Noll.Compiler.Transform.TreeSpec
import qualified Noll.Compiler.Transform.Type.AliasExpansionSpec
import qualified Noll.CompilerExamples.Test01
import qualified Noll.CompilerExamples.Test02
import qualified Noll.CompilerSpec
import qualified Noll.Core.CompilerSpec
import qualified Noll.Core.Language.Expr.ReplaceSpec
import qualified Noll.TypeSystem.Constraint.GenerationSpec
import qualified Noll.TypeSystem.UnificationSpec
import qualified Noll.TypeSystemExamples.Test01
import qualified Noll.TypeSystemExamples.Test02
import qualified Noll.TypeSystemExamples.Test03
import qualified Noll.TypeSystemExamples.Test04
import qualified Noll.TypeSystemExamples.Test05
import qualified Noll.TypeSystemExamples.Test06
import qualified Noll.TypeSystemExamples.Test07
import qualified Noll.TypeSystemExamples.Test08
import qualified Noll.TypeSystemExamples.Test09
import qualified Noll.TypeSystemExamples.Test10
import qualified Noll.TypeSystemExamples.Test11
import qualified Noll.TypeSystemExamples.Test12
import qualified Noll.TypeSystemExamples.Test13
import qualified Noll.TypeSystemExamples.Test14
import qualified Noll.TypeSystemExamples.Test15
import qualified Noll.TypeSystemSpec
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Noll.Language.Type.IndexedSpec.spec
    Noll.TypeSystem.Constraint.GenerationSpec.spec
    Noll.Compiler.Transform.Pattern.DesugarSpec.spec
    Noll.TypeSystemSpec.spec
    Noll.TypeSystem.UnificationSpec.spec
    Noll.TypeSystemExamples.Test01.spec
    Noll.TypeSystemExamples.Test02.spec
    Noll.TypeSystemExamples.Test03.spec
    Noll.TypeSystemExamples.Test04.spec
    Noll.TypeSystemExamples.Test05.spec
    Noll.TypeSystemExamples.Test06.spec
    Noll.TypeSystemExamples.Test07.spec
    Noll.TypeSystemExamples.Test08.spec
    Noll.TypeSystemExamples.Test09.spec
    Noll.TypeSystemExamples.Test10.spec
    Noll.TypeSystemExamples.Test11.spec
    Noll.TypeSystemExamples.Test12.spec
    Noll.TypeSystemExamples.Test13.spec
    Noll.TypeSystemExamples.Test14.spec
    Noll.TypeSystemExamples.Test15.spec
    Noll.Compiler.PatternMatchingSpec.spec
    Lang.FreeVarsSpec.spec
    Noll.Compiler.Transform.TreeSpec.spec
    Noll.Compiler.PatternMatchingExamples.Test01.spec
    Noll.Compiler.PatternMatchingExamples.Test02.spec
    Noll.Compiler.Transform.Pattern.OrExpansionSpec.spec
    Noll.Compiler.Transform.Type.AliasExpansionSpec.spec
    Noll.Compiler.Transform.FoldSpec.spec
    Noll.CompilerExamples.Test01.spec
    Noll.CompilerExamples.Test02.spec
    --    Noll.CompilerSpec.spec
    Lang.UtilsSpec.spec
    Noll.Compiler.NormalizeObjectsSpec.spec
    Noll.Core.Language.Expr.ReplaceSpec.spec
    Noll.Core.CompilerSpec.spec

--    Noll.TypeSystem.TypeConstraint.CollectSpec.spec
--    Noll.TypeSystem.TypeSubstitutionSpec.spec
--    Noll.TypeSystem.KindSubstitutionSpec.spec
--    Noll.TypeSystem.ConstraintSolverSpec.spec
--    Noll.TypeSystem.KindConstraint.CollectSpec.spec
--    Noll.TypeSystemSpec.spec
