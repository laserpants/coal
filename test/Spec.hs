import qualified Noll.TypeSystem.Constraint.CollectSpec
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Noll.TypeSystem.Constraint.CollectSpec.spec
