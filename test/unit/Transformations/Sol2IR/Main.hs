module Transformations.Sol2IR.Main where

import Test.Tasty
import qualified Transformations.Sol2IR.Expression
import qualified Transformations.Sol2IR.Statement
import qualified Transformations.Sol2IR.Type
import qualified Transformations.Sol2IR.Variable
import qualified Transformations.Sol2IR.Function
import qualified Transformations.Sol2IR.Contract
import qualified Transformations.Sol2IR.Program
import qualified Transformations.Sol2IR.Identifier
import qualified Transformations.Sol2IR.Helper
import qualified IR as Transformations.Sol2IR

spec :: IO TestTree
spec =
  testGroup "Transform Solidity to IR Tests"
    <$> sequence
      [ Transformations.Sol2IR.Type.spec,
        Transformations.Sol2IR.Statement.spec,
        Transformations.Sol2IR.Expression.spec,
        Transformations.Sol2IR.Variable.spec,
        Transformations.Sol2IR.Function.spec,
        Transformations.Sol2IR.Contract.spec,
        Transformations.Sol2IR.Identifier.spec,
        Transformations.Sol2IR.Program.spec,
        Transformations.Sol2IR.Helper.spec
      ]
