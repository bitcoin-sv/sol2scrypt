module TransformerSpec where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.IntM2Scr.Main
import Transformations.Sol2IntM.Main

spec :: IO TestTree
spec =
  testGroup "Transformer"
    <$> sequence
      [ Transformations.Sol2IntM.Main.spec,
        Transformations.IntM2Scr.Main.spec
      ]
