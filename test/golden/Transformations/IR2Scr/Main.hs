module Transformations.IR2Scr.Main where

import Test.Tasty
import qualified Transformations.IR2Scr.Expression
import qualified Transformations.IR2Scr.Type

spec :: IO TestTree
spec =
  testGroup "Transform IR to sCrypt Tests"
    <$> sequence
      [ Transformations.IR2Scr.Type.spec,
        Transformations.IR2Scr.Expression.spec
      ]
