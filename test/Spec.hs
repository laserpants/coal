import qualified Noll.Language.IndexedSpec

-- import qualified Noll.TypeSystem.ConstraintSolverSpec
-- import qualified Noll.TypeSystem.KindConstraint.CollectSpec
-- import qualified Noll.TypeSystem.KindSubstitutionSpec
-- import qualified Noll.TypeSystem.TypeConstraint.CollectSpec
-- import qualified Noll.TypeSystem.TypeSubstitutionSpec

import qualified Noll.TypeSystem.Constraint.AggregationSpec
import qualified Noll.TypeSystem.UnificationSpec
import qualified Noll.TypeSystemSpec
import Test.Hspec (hspec, it)

main :: IO ()
main =
  hspec $ do
    Noll.Language.IndexedSpec.spec
    Noll.TypeSystem.Constraint.AggregationSpec.spec
    Noll.TypeSystemSpec.spec
    Noll.TypeSystem.UnificationSpec.spec

--    Noll.TypeSystem.TypeConstraint.CollectSpec.spec
--    Noll.TypeSystem.TypeSubstitutionSpec.spec
--    Noll.TypeSystem.KindSubstitutionSpec.spec
--    Noll.TypeSystem.ConstraintSolverSpec.spec
--    Noll.TypeSystem.KindConstraint.CollectSpec.spec
--    Noll.TypeSystemSpec.spec
