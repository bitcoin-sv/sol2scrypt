{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Transformations.Sol2IR.Program where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper
import Text.RawString.QQ

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable SolidityCode Program'" $ do
  let itProgram sol e = it "should transfrom Solidity `SolidityCode` to IR Program correctly" $ do
        r1 <- sol2Ir sol2Program sol
        r1 `shouldBe` Just e

  describe "#Empty Program" $ do
    itProgram  ""  ( IR.Program [] [] [])

  describe "#Simple Program" $ do
    itProgram  [r|
contract SimpleStorage {
  uint storedData;

  constructor() {}
}   
|]  (Program {programImports = [], programContracts = [Contract {contractName = (IR.Identifier "SimpleStorage"), contractBody = [IR.StateVariableDeclaration (StateVariable {stateVarName = (IR.Identifier "storedData"), stateVarType = ElementaryType Int, stateVisibility = Default, stateInitialValue = Nothing}),ConstructorDefinition (Constructor {ctorParams = ParamList [], ctorBody = IR.Block []})]}], programLibraries = []})