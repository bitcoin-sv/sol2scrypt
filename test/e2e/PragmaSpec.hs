{-# LANGUAGE ScopedTypeVariables #-}

module PragmaSpec where
import Transpiler
import Solidity as Sol
import IR as IR
import Scrypt as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "Transpile Pragma" $ do

    it "should transpile Solidity `Pragma` correctly" $ do
        tr :: TranspileResult Sol.PragmaDirective IEmpty' (Maybe Scr.Empty) <- transpile "pragma solidity ^0.8.10;"
        scryptCode tr `shouldBe` ""