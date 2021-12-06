module GeneratorSpec where

import Scrypt.Generator
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

import qualified Generables.Type
import qualified Generables.Expr

baseSpec :: IO TestTree
baseSpec = testSpec "#generateScrypt" $ do
  it "should generate empty code if nothing is given" $ do
    r <- generateScrypt (Nothing :: Maybe Scr.Type)
    r `shouldBe` ""

spec :: IO TestTree
spec =
  testGroup "Generator"
    <$> sequence
      [ baseSpec,
        Generables.Type.spec,
        Generables.Expr.spec
      ]
