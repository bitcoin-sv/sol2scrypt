{-# LANGUAGE ScopedTypeVariables #-}

module FunctionSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Function" $ do
  let itTranspile title sol scr = it ("should transpile Solidity " ++ title ++ " correctly") $ do
        tr :: TranspileResult Sol.ContractPart IFunction' (Maybe (Scr.Function Ann)) <- transpile sol
        scryptCode tr `shouldBe` scr

  itTranspile
    "external function"
    "function set(uint x) external { storedData = x; }"
    "public function set(int x, SigHashPreimage txPreimage) { storedData = x; require(true); }"

  itTranspile
    "public view function"
    "function get() public view returns (uint) { return storedData; }"
    "function get() : int { return storedData; }"