import GeneratorSpec
import Test.Tasty
import TransformerSpec

main :: IO ()
main = defaultMain =<< unitTests

unitTests :: IO TestTree
unitTests =
  testGroup "unit tests"
    <$> sequence [TransformerSpec.spec, GeneratorSpec.spec]
