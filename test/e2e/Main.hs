import Test.Tasty
import TypeSpec
import ExpressionSpec

main :: IO ()
main = defaultMain =<< e2eTests

e2eTests :: IO TestTree
e2eTests =
  testGroup "e2e tests"
    <$> sequence [TypeSpec.spec, ExpressionSpec.spec]