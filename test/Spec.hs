import qualified Noll.Language.TypeIndexedSpec
import qualified Noll.TypeSystem.ConstraintSolverSpec
import qualified Noll.TypeSystem.KindConstraint.CollectSpec
import qualified Noll.TypeSystem.KindSubstitutionSpec
import qualified Noll.TypeSystem.TypeConstraint.CollectSpec
import qualified Noll.TypeSystem.TypeSubstitutionSpec
import qualified Noll.TypeSystem.TypeUnificationSpec
import qualified Noll.TypeSystemSpec
import Test.Hspec (hspec, it)

main :: IO ()
main =
  hspec $ do
    Noll.TypeSystem.TypeConstraint.CollectSpec.spec
    Noll.Language.TypeIndexedSpec.spec
    Noll.TypeSystem.TypeSubstitutionSpec.spec
    Noll.TypeSystem.KindSubstitutionSpec.spec
    Noll.TypeSystem.ConstraintSolverSpec.spec
    Noll.TypeSystem.KindConstraint.CollectSpec.spec
    Noll.TypeSystem.TypeUnificationSpec.spec
    Noll.TypeSystemSpec.spec
