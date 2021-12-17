module Transformations.Sol2IR.Main where

import Test.Tasty
import qualified Transformations.Sol2IR.Expression
import qualified Transformations.Sol2IR.Type

spec :: IO TestTree
spec =
  testGroup "Transform Solidity to IR Tests"
    <$> sequence
      [ Transformations.Sol2IR.Type.spec,
        Transformations.Sol2IR.Expression.spec
      ]
