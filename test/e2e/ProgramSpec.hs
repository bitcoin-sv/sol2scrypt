{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProgramSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Text.RawString.QQ
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Program" $ do
  let itProgram sol scrypt = it "should transpile Solidity SolidityCode correctly" $ do
        tr :: TranspileResult SolidityCode IProgram' (Maybe (Scr.Program Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt


  itProgram
      [r|
      
contract SimpleStorage {
    uint storedData;

    constructor() {}
}|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  constructor() {
  }
}|]

