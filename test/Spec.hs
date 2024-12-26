import qualified Noll.Language.HasTypeIndexesSpec
import qualified Noll.TypeSystem.KindConstraint.CollectSpec
import qualified Noll.TypeSystem.KindSubstitutionSpec
import qualified Noll.TypeSystem.SolverSpec
import qualified Noll.TypeSystem.TypeConstraint.CollectSpec
import qualified Noll.TypeSystem.TypeSubstitutionSpec
import qualified Noll.TypeSystemSpec
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Noll.TypeSystem.TypeConstraint.CollectSpec.spec
    Noll.Language.HasTypeIndexesSpec.spec
    Noll.TypeSystem.TypeSubstitutionSpec.spec
    Noll.TypeSystem.KindSubstitutionSpec.spec
    Noll.TypeSystem.SolverSpec.spec
    Noll.TypeSystem.KindConstraint.CollectSpec.spec
    Noll.TypeSystemSpec.spec
