module Generables.Program where

import Scrypt.Generables.Base
import Scrypt.Generables.Variable ()
import Scrypt.Generables.Program ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "Program" $ do
  describe "#Program" $ do
    it "instance Generable (Maybe (Scr.Program Ann))" $ do
      r <- generateScrypt (CodeGenState 0) (Just (Scr.Program [] [] [] [] nil))
      r `shouldBe` ""

    it "instance Generable (Maybe (Scr.Program Ann))" $ do
      r <- generateScrypt (CodeGenState 0) (Just (Scr.Program [ImportPath "./myLovelyLovelyLib.scrypt" (Ann {unAnn = Nothing})] [] [] [] nil))
      r `shouldBe` "import \"./myLovelyLovelyLib.scrypt\";\n\n"