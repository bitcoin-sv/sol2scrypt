module TransformerSpec where

import Test.Tasty
import Transformations.IR2Scr.Main
import Transformations.Sol2IR.Main

spec :: IO TestTree
spec =
  testGroup "Transformer"
    <$> sequence
      [ Transformations.Sol2IR.Main.spec,
        Transformations.IR2Scr.Main.spec
      ]
