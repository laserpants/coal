import qualified Noll.Language.IndexedSpec

-- import qualified Noll.SystemF.ConstraintSolverSpec
-- import qualified Noll.SystemF.KindConstraint.CollectSpec
-- import qualified Noll.SystemF.KindSubstitutionSpec
-- import qualified Noll.SystemF.TypeConstraint.CollectSpec
-- import qualified Noll.SystemF.TypeSubstitutionSpec

import qualified Noll.Compiler.PatternMatchingExamples.Test01
import qualified Noll.Compiler.PatternMatchingExamples.Test02
import qualified Noll.Compiler.PatternMatchingSpec
import qualified Noll.Compiler.Transform.Pattern.AnyOrExpansionSpec
import qualified Noll.Compiler.Transform.Pattern.ExpansionSpec
import qualified Noll.Compiler.Transform.TreeSpec
import qualified Noll.Language.HasFreeSpec
import qualified Noll.SystemF.Constraint.GenerationSpec
import qualified Noll.SystemF.UnificationSpec
import qualified Noll.SystemFExamples.Test01
import qualified Noll.SystemFExamples.Test02
import qualified Noll.SystemFExamples.Test03
import qualified Noll.SystemFExamples.Test04
import qualified Noll.SystemFExamples.Test05
import qualified Noll.SystemFExamples.Test06
import qualified Noll.SystemFExamples.Test07
import qualified Noll.SystemFExamples.Test08
import qualified Noll.SystemFExamples.Test09
import qualified Noll.SystemFExamples.Test10
import qualified Noll.SystemFExamples.Test11
import qualified Noll.SystemFExamples.Test12
import qualified Noll.SystemFExamples.Test13
import qualified Noll.SystemFExamples.Test14
import qualified Noll.SystemFExamples.Test15
import qualified Noll.SystemFSpec
import Test.Hspec (hspec, it)

main :: IO ()
main =
  hspec $ do
    Noll.Language.IndexedSpec.spec
    Noll.SystemF.Constraint.GenerationSpec.spec
    Noll.Compiler.Transform.Pattern.ExpansionSpec.spec
    Noll.SystemFSpec.spec
    Noll.SystemF.UnificationSpec.spec
    Noll.SystemFExamples.Test01.spec
    Noll.SystemFExamples.Test02.spec
    Noll.SystemFExamples.Test03.spec
    Noll.SystemFExamples.Test04.spec
    Noll.SystemFExamples.Test05.spec
    Noll.SystemFExamples.Test06.spec
    Noll.SystemFExamples.Test07.spec
    Noll.SystemFExamples.Test08.spec
    Noll.SystemFExamples.Test09.spec
    Noll.SystemFExamples.Test10.spec
    Noll.SystemFExamples.Test11.spec
    Noll.SystemFExamples.Test12.spec
    Noll.SystemFExamples.Test13.spec
    Noll.SystemFExamples.Test14.spec
    Noll.SystemFExamples.Test15.spec
    Noll.Compiler.PatternMatchingSpec.spec
    Noll.Language.HasFreeSpec.spec
    Noll.Compiler.Transform.TreeSpec.spec
    Noll.Compiler.PatternMatchingExamples.Test01.spec
    Noll.Compiler.PatternMatchingExamples.Test02.spec
--    Noll.Compiler.Transform.Pattern.AnyOrExpansionSpec.spec

--    Noll.SystemF.TypeConstraint.CollectSpec.spec
--    Noll.SystemF.TypeSubstitutionSpec.spec
--    Noll.SystemF.KindSubstitutionSpec.spec
--    Noll.SystemF.ConstraintSolverSpec.spec
--    Noll.SystemF.KindConstraint.CollectSpec.spec
--    Noll.SystemFSpec.spec
