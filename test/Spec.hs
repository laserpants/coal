import qualified Noll.Language.HasTypeIndexesSpec
import qualified Noll.TypeSystem.Constraint.CollectSpec
import qualified Noll.TypeSystem.SubstitutionSpec
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Noll.TypeSystem.Constraint.CollectSpec.spec
    Noll.Language.HasTypeIndexesSpec.spec
    Noll.TypeSystem.SubstitutionSpec.spec
