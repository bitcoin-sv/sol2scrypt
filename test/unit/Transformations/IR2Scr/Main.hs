module Transformations.IR2Scr.Main where

import Test.Tasty
import qualified Transformations.IR2Scr.Expression
import qualified Transformations.IR2Scr.Function
import qualified Transformations.IR2Scr.Stmt
import qualified Transformations.IR2Scr.Type
import qualified Transformations.IR2Scr.Variable

spec :: IO TestTree
spec =
  testGroup "Transform IR to sCrypt Tests"
    <$> sequence
      [ Transformations.IR2Scr.Type.spec,
        Transformations.IR2Scr.Stmt.spec,
        Transformations.IR2Scr.Expression.spec,
        Transformations.IR2Scr.Variable.spec,
        Transformations.IR2Scr.Function.spec
      ]
