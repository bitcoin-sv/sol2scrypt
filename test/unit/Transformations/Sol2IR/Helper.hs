{-# LANGUAGE FlexibleContexts #-}

module Transformations.Sol2IR.Helper where

import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper
import IR.Transformations.Sol2IR.Helper
import Solidity.Spec as Sol

spec :: IO TestTree
spec = testSpec "Sol2IR Helper" $ do
  let itCheckEIE expr expr' = it ("should check `" ++ expr ++ "` expression's existance in expression correctly") $ do
        e <- sol2Expr expr
        e' <- sol2Expr expr'
        exprExistsInExpr e e' `shouldBe` True

  let itCheckEIS expr stmt = it ("should check `" ++ expr ++ "` expression's existance in statement correctly") $ do
        e <- sol2Expr expr
        s <- sol2Stmt stmt
        exprExistsInStmt e s `shouldBe` True

  describe "#exprExistsInExpr" $ do
    itCheckEIE "msg.value" "-msg.value"
    itCheckEIE "msg.value" "2 + msg.value"
    itCheckEIE "msg.value" "msg.value > 0 ? 1 : 2"
    itCheckEIE "msg.value" "f(2, g(msg.value))"

  describe "#exprExistsInStmt" $ do
    itCheckEIS "msg.sender" "msg.sender;"
    itCheckEIS "msg.sender" "if (msg.sender) true;"
    itCheckEIS "msg.sender" "address a = msg.sender;"
    itCheckEIS "msg.sender" "if (x > 0) true; else { msg.sender; }"
    itCheckEIS "msg.sender" "f(msg.sender);"
    itCheckEIS "msg.sender" "{{{msg.sender;}}}"

