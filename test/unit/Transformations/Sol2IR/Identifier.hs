{-# LANGUAGE FlexibleContexts #-}

module Transformations.Sol2IR.Identifier where

import IR.Spec as IR
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Identifier IIdentifier'" $ do
  let itIdentifier sol e = it ("should transfrom Solidity `" ++ sol ++ "` to IR Type correctly") $ do
        r1 <- sol2Ir sol2Identifier sol
        r1 `shouldBe` Just (IR.Identifier e)

  describe "#Identifier" $ do
    itIdentifier "a" "a"
    itIdentifier "A" "A"
    itIdentifier "Aabc" "Aabc"
    itIdentifier "A_adsf" "A_adsf"
    itIdentifier "aaaAfsdfas_" "aaaAfsdfas_"
    itIdentifier "$asdfas" "$asdfas"
