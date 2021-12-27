{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.Sol2IR.Contract where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Contractpart IContractBodyElement'" $ do
  let itProperty solidityCode target = it "should transfrom Solidity `ContractPartStateVariableDeclaration` to IR Function correctly" $ do
        ir :: IContractBodyElement' <- sol2Ir sol2ContractPart solidityCode
        ir `shouldBe` target

  describe "when the function is `public pure`" $ do
    itProperty "uint a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Default Nothing))
    itProperty "uint private a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Private Nothing))
    itProperty "uint public a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Public Nothing))
    itProperty "int public a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Public Nothing))

