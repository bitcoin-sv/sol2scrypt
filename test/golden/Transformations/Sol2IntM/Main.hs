module Transformations.Sol2IntM.Main where

import Test.Tasty
import qualified Transformations.Sol2IntM.Expression
import qualified Transformations.Sol2IntM.Type

spec :: IO TestTree
spec =
  testGroup "Transform Solidity to Intermediate Tests"
    <$> sequence
      [ Transformations.Sol2IntM.Type.spec,
        Transformations.Sol2IntM.Expression.spec
      ]
