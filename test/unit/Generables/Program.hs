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
      r <- generateScrypt (CodeGenState 0) (Just (Scr.Param (TypeAnn Scr.Bool nil) (NameAnn "p" nil) (Const False) Nothing Default (IsStateProp False) nil))
      r `shouldBe` "bool p"