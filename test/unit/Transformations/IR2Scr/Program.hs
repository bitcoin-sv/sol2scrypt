{-# LANGUAGE FlexibleContexts #-}

module Transformations.IR2Scr.Program where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IProgram Program" $ do
  let itProgram title e1 e2 = it ("should transfrom IR `" ++ title ++ "` to sCrypt Program correctly") $ do
        r <- transform2Scrypt $ Just e1
        r `shouldBe` Just e2

  describe "#Program" $ do
    itProgram "Empty Program" (IR.Program [] [] [] []) (Scr.Program [] [] [] [] nil)