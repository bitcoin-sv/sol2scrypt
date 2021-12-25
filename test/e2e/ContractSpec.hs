{-# LANGUAGE ScopedTypeVariables #-}

module ContractSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Contract" $ do
  let itProperty sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr :: TranspileResult ContractPart IContractBodyElement' (Maybe (Scr.Param Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt
  describe "#Contract" $ do
    describe "#Property" $ do
      itProperty "uint storedData;" "@state int storedData;"
      itProperty "int storedData;" "@state int storedData;"
      itProperty "bool a;" "@state bool a;"
      itProperty "bytes a;" "@state bytes a;"
