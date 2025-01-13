import qualified Noll.Language.IndexedSpec

-- import qualified Noll.TypeSystem.ConstraintSolverSpec
-- import qualified Noll.TypeSystem.KindConstraint.CollectSpec
-- import qualified Noll.TypeSystem.KindSubstitutionSpec
-- import qualified Noll.TypeSystem.TypeConstraint.CollectSpec
-- import qualified Noll.TypeSystem.TypeSubstitutionSpec

import qualified Noll.TypeSystem.Constraint.GenerationSpec
import qualified Noll.TypeSystem.UnificationSpec
import qualified Noll.TypeSystemExamples.Test1
import qualified Noll.TypeSystemExamples.Test2
import qualified Noll.TypeSystemExamples.Test3
import qualified Noll.TypeSystemExamples.Test4
import qualified Noll.TypeSystemExamples.Test5
import qualified Noll.TypeSystemExamples.Test6
import qualified Noll.TypeSystemExamples.Test7
import qualified Noll.TypeSystemExamples.Test8
import qualified Noll.TypeSystemExamples.Test9
import qualified Noll.TypeSystemSpec
import Test.Hspec (hspec, it)

main :: IO ()
main =
  hspec $ do
    Noll.Language.IndexedSpec.spec
    Noll.TypeSystem.Constraint.GenerationSpec.spec
    Noll.TypeSystemSpec.spec
    Noll.TypeSystem.UnificationSpec.spec
    Noll.TypeSystemExamples.Test1.spec
    Noll.TypeSystemExamples.Test2.spec
    Noll.TypeSystemExamples.Test3.spec
    Noll.TypeSystemExamples.Test4.spec
    Noll.TypeSystemExamples.Test5.spec
    Noll.TypeSystemExamples.Test6.spec
    Noll.TypeSystemExamples.Test7.spec
    Noll.TypeSystemExamples.Test8.spec
    Noll.TypeSystemExamples.Test9.spec

--    Noll.TypeSystem.TypeConstraint.CollectSpec.spec
--    Noll.TypeSystem.TypeSubstitutionSpec.spec
--    Noll.TypeSystem.KindSubstitutionSpec.spec
--    Noll.TypeSystem.ConstraintSolverSpec.spec
--    Noll.TypeSystem.KindConstraint.CollectSpec.spec
--    Noll.TypeSystemSpec.spec
