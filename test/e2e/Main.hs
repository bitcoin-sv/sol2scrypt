import Test.Tasty
import TypeSpec
import ExpressionSpec
import StmtSpec
import PragmaSpec
import VariableSpec
import FunctionSpec

main :: IO ()
main = defaultMain =<< e2eTests

e2eTests :: IO TestTree
e2eTests =
  testGroup "e2e tests"
    <$> sequence [TypeSpec.spec, StmtSpec.spec, ExpressionSpec.spec, PragmaSpec.spec, VariableSpec.spec, FunctionSpec.spec]
