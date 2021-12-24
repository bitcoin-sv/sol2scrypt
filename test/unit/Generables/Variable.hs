module Generables.Variable where

import Scrypt.Generables.Base
import Scrypt.Generables.Variable ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "Variables" $ do
  describe "#Parameter" $ do
    it "instance Generable (Maybe (Scr.Param Ann))" $ do
      genCode (Just (Scr.Param (TypeAnn Scr.Bool nil) (NameAnn "p" nil) (Const False) Nothing Default (IsStateProp False) nil)) 
        `shouldBe` "bool p"