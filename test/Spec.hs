import qualified Noll.Language.HasTypeIndexesSpec
import qualified Noll.TypeSystem.TypeConstraint.CollectSpec
import qualified Noll.TypeSystem.TypeConstraint.SolverSpec
import qualified Noll.TypeSystem.SubstitutionSpec
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Noll.TypeSystem.TypeConstraint.CollectSpec.spec
    Noll.Language.HasTypeIndexesSpec.spec
    Noll.TypeSystem.SubstitutionSpec.spec
    Noll.TypeSystem.TypeConstraint.SolverSpec.spec
