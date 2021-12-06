import GeneratorSpec
import Test.Tasty
import TransformerSpec

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests =
  testGroup "golden tests"
    <$> sequence [TransformerSpec.spec, GeneratorSpec.spec]
