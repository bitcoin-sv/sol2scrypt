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
      r <- generateScrypt (CodeGenState 0) (Just (Scr.Param (TypeAnn Scr.Bool nil) (NameAnn "p" nil) (Const False) Nothing Default (IsStateProp False) nil))
      r `shouldBe` "bool p"

    it "instance Generable (Maybe (Scr.Static Ann))" $ do
      r <- generateScrypt (CodeGenState 0) (Just (Scr.Static (Scr.Param (TypeAnn Scr.Int nil) (NameAnn "a" nil) (Const True) Nothing Scr.Public (IsStateProp False) nil) (Scr.IntLiteral False 8 nil) nil))
      r `shouldBe` "\nstatic const int a = 8;"