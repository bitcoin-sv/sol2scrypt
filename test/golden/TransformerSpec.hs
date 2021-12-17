module TransformerSpec where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.IR2Scr.Main
import Transformations.Sol2IR.Main

spec :: IO TestTree
spec =
  testGroup "Transformer"
    <$> sequence
      [ Transformations.Sol2IR.Main.spec,
        Transformations.IR2Scr.Main.spec
      ]
