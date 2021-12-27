{-# LANGUAGE FlexibleContexts #-}

module Transformations.IR2Scr.Identifier where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils (nil)

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IIdentifier (Maybe NameAnn Ann)" $ do
  let itIdentifier a b = it ("should transfrom IR `" ++ a ++ "` to sCrypt Type correctly") $ do
        r <- transform2Scrypt (Just $ IR.Identifier a)
        r `shouldBe` Just (NameAnn b nil)

  describe "#Identifier" $ do
    itIdentifier "a" "a"
    itIdentifier "A" "A"
    itIdentifier "Aabc" "Aabc"
    itIdentifier "A_adsf" "A_adsf"
    itIdentifier "aaaAfsdfas_" "aaaAfsdfas_"
    itIdentifier "$asdfas" "_asdfas"
