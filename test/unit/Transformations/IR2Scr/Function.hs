{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.IR2Scr.Function where

import qualified IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IFunction (Scr.Function Ann)" $ do
  let itTransformFunc f1 f2 = it "should transfrom IR `IFunction` to sCrypt Function correctly" $ do
        r :: Maybe (Scr.Function Ann) <- transform2Scrypt f1
        r `shouldBe` f2

  let nonPubFunc1 vis = Just (IR.Function {IR.funcName = IR.Identifier "add", IR.funcParams = IR.ParamList [IR.Param {IR.paramType = IR.ElementaryType IR.Int, IR.paramName = IR.Identifier "x"}, IR.Param {IR.paramType = IR.ElementaryType IR.Int, IR.paramName = IR.Identifier "y"}], IR.funcBody = IR.Block [IR.ReturnStmt {IR.retExpr = IR.LiteralExpr (IR.BoolLiteral True)}], IR.funcReturn = IR.ElementaryType IR.Bool, IR.funcVisibility = vis, IR.funcStatic = False})
      pubFunc1 vis = Just (IR.Function {IR.funcName = IR.Identifier "add", IR.funcParams = IR.ParamList [IR.Param {IR.paramType = IR.ElementaryType IR.Int, IR.paramName = IR.Identifier "x"}, IR.Param {IR.paramType = IR.ElementaryType IR.Int, IR.paramName = IR.Identifier "y"}], IR.funcBody = IR.Block [IR.RequireStmt {IR.verifyExpr = IR.LiteralExpr (IR.BoolLiteral True)}], IR.funcReturn = IR.ElementaryType IR.Bool, IR.funcVisibility = vis, IR.funcStatic = False})

  describe "to non-public function" $ do
    itTransformFunc (nonPubFunc1 IR.Private) $ expNonPubFunc1 Scr.Private
    itTransformFunc (nonPubFunc1 IR.Default) $ expNonPubFunc1 Scr.Default

  describe "to public function" $ do
    itTransformFunc (pubFunc1 IR.Public) $ expPubFunc1 Scr.Public

  where
    expNonPubFunc1 vis =
        Just
          ( Function
              (NameAnn "add" nil)
              [ Param
                  (TypeAnn Int nil)
                  (NameAnn "x" nil)
                  (Const False)
                  Nothing
                  Default
                  (IsStateProp False)
                  nil,
                Param
                  (TypeAnn Int nil)
                  (NameAnn "y" nil)
                  (Const False)
                  Nothing
                  Default
                  (IsStateProp False)
                  nil
              ]
              (TypeAnn Bool nil)
              ( RegularBody
                  [ ReturnStmt
                      (BoolLiteral True nil)
                      nil
                  ]
                  (BoolLiteral True nil)
                  nil
              )
              vis
              (Stc False)
              nil
          )

    expPubFunc1 vis =
        Just
          ( Function
              (NameAnn "add" nil)
              [ Param
                  (TypeAnn Int nil)
                  (NameAnn "x" nil)
                  (Const False)
                  Nothing
                  Default
                  (IsStateProp False)
                  nil,
                Param
                  (TypeAnn Int nil)
                  (NameAnn "y" nil)
                  (Const False)
                  Nothing
                  Default
                  (IsStateProp False)
                  nil
              ]
              (TypeAnn Bool nil)
              ( RegularBody
                  [ Require
                      (BoolLiteral True nil)
                      nil
                  ]
                  (BoolLiteral True nil)
                  nil
              )
              vis
              (Stc False)
              nil
          )
