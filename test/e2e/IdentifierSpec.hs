{-# LANGUAGE ScopedTypeVariables #-}

module IdentifierSpec where


import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Identifier" $ do
  let itIdentifier sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr :: TranspileResult Sol.Identifier IR.IIdentifier' (Maybe (NameAnn Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt
  describe "#Identifier" $ do
    itIdentifier "aaa" "aaa"
    itIdentifier "aZ_$0" "aZ__0"
    itIdentifier "a" "a"
    itIdentifier "Af" "Af"
    itIdentifier "SigHashPreimage" "userDefined_SigHashPreimage"
    itIdentifier "$aa" "_aa"
    itIdentifier "A4535aa$" "A4535aa_"

