module Transformations.IntM2Scr.Main where

import Test.Tasty
import qualified Transformations.IntM2Scr.Expression
import qualified Transformations.IntM2Scr.Type

spec :: IO TestTree
spec =
  testGroup "Transform IR to sCrypt Tests"
    <$> sequence
      [ Transformations.IntM2Scr.Type.spec,
        Transformations.IntM2Scr.Expression.spec
      ]
