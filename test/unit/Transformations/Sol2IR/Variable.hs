{-# LANGUAGE FlexibleContexts #-}
module Transformations.Sol2IR.Variable where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper

spec :: IO TestTree
spec = testSpec "Variables" $ do
  let itParameter sol t = it ("should transfrom Solidity `" ++ sol ++ "` to IR Type correctly") $ do
        r1 <- sol2Ir sol2Parameter sol
        r1 `shouldBe` Just (IR.Param t (IR.Identifier "a"))
  describe "#Parameter" $ do
    itParameter "bool a" (ElementaryType Bool)
    itParameter "int a" (ElementaryType Int)
    itParameter "int8 a" (ElementaryType Int)
    itParameter "int256 a" (ElementaryType Int)
    itParameter "uint a" (ElementaryType Int)
    itParameter "uint8 a" (ElementaryType Int)
    itParameter "uint256 a" (ElementaryType Int)
    itParameter "byte a" (ElementaryType Bytes)
    itParameter "bytes a" (ElementaryType Bytes)
    itParameter "bytes1 a" (ElementaryType Bytes)
    itParameter "bytes32 a" (ElementaryType Bytes)
    itParameter "address a" (ElementaryType Address)

