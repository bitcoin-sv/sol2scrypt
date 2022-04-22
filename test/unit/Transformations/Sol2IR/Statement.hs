{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.Sol2IR.Statement where

import IR.Spec as IR
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Sol.Statement IExpr'" $ do
  let itstmt title sol e2 = it ("should transfrom Solidity `" ++ title ++ "` to IR Statement correctly") $ do
        ir <- sol2Ir sol2Stmt sol
        ir `shouldBe` Just e2
  let itstmt' title sol = it ("should transfrom Solidity `" ++ title ++ "` to IR Statement correctly") $ do
        ir :: IStatement' <- sol2Ir sol2Stmt sol
        ir `shouldBe` Nothing

  describe "#SimpleStatementExpression" $ do
    itstmt
      "BoolLiteral"
      "true;"
      (ExprStmt (LiteralExpr $ IR.BoolLiteral True))

    itstmt
      "BoolLiteral"
      "true;"
      (ExprStmt (LiteralExpr $ IR.BoolLiteral True))

    itstmt
      "NumberLiteral"
      "0x0123abcdef;"
      (ExprStmt (LiteralExpr $ IR.IntLiteral True 4893429231))

    itstmt
      "NumberLiteral"
      "12345;"
      (ExprStmt (LiteralExpr $ IR.IntLiteral False 12345))


    itstmt
      "HexLiteral"
      "hex\"010113\";"
      (ExprStmt (LiteralExpr $ IR.BytesLiteral [1,1,19]))

    itstmt
      "Unary"
      "-100;"
      (ExprStmt (UnaryExpr Negate (LiteralExpr $ IR.IntLiteral False 100)))


    itstmt
      "Binary"
      "100 * 1;"
      (ExprStmt (BinaryExpr IR.Mul (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1)))

    describe "#AssignStmt" $ do
      itstmt
        "IntLiteral"
        "x = 11;"
        (IR.AssignStmt [IR.IdentifierExpr $ IR.Identifier "x"] [LiteralExpr $ IR.IntLiteral False 11])

      itstmt
        "IntLiteral"
        "x = 0x11;"
        (IR.AssignStmt [IR.IdentifierExpr $ IR.Identifier "x"] [LiteralExpr $ IR.IntLiteral True 17])

      itstmt
        "BooleanLiteral"
        "x = true;"
        (IR.AssignStmt [IR.IdentifierExpr $ IR.Identifier "x"] [LiteralExpr $ IR.BoolLiteral True])

      itstmt
        "BooleanLiteral"
        "x = false;"
        (IR.AssignStmt [IR.IdentifierExpr $ IR.Identifier "x"] [LiteralExpr $ IR.BoolLiteral False])


      itstmt
        "HexLiteral"
        "x = hex\"010113\";"
        (IR.AssignStmt [IR.IdentifierExpr $ IR.Identifier "x"] [LiteralExpr $ IR.BytesLiteral [1,1,19]])

      itstmt
        "HexLiteral"
        "x = hex\"\";"
        (IR.AssignStmt [IR.IdentifierExpr $ IR.Identifier "x"] [LiteralExpr $ IR.BytesLiteral []])

    describe "#DeclareStmt" $ do

      itstmt
        "IntLiteral"
        "int x = 11;"
        (IR.DeclareStmt [IR.Param (ElementaryType Int) (IR.Identifier "x")] [LiteralExpr $ IR.IntLiteral False 11])

      itstmt
        "IntLiteral"
        "int x = 0x11;"
        (IR.DeclareStmt [IR.Param (ElementaryType Int) (IR.Identifier "x")] [LiteralExpr $ IR.IntLiteral True 17])

      itstmt
        "BooleanLiteral"
        "bool x = true;"
        (IR.DeclareStmt [IR.Param (ElementaryType Bool) (IR.Identifier "x")] [LiteralExpr $ IR.BoolLiteral True])

      itstmt
        "BooleanLiteral"
        "bool x = false;"
        (IR.DeclareStmt [IR.Param (ElementaryType Bool) (IR.Identifier "x")] [LiteralExpr $ IR.BoolLiteral False])


      itstmt
        "HexLiteral"
        "bytes x = hex\"010113\";"
        (IR.DeclareStmt [IR.Param (ElementaryType Bytes) (IR.Identifier "x")] [LiteralExpr $ IR.BytesLiteral [1,1,19]])

      itstmt
        "HexLiteral"
        "bytes x = hex\"\";"
        (IR.DeclareStmt [IR.Param (ElementaryType Bytes) (IR.Identifier "x")][LiteralExpr $ IR.BytesLiteral []])


      describe "#BlockStmt" $ do
        itstmt
          "contains DeclareStmt "
          "{ bytes x = hex\"\"; }"
           (IR.BlockStmt (IR.Block [IR.DeclareStmt [IR.Param (ElementaryType Bytes) (IR.Identifier "x")][LiteralExpr $ IR.BytesLiteral []]]))
        
        itstmt
          "contains DeclareStmt AssignStmt ExprStmt"
          "{ int x = 3; x++; x = 1; }"
          (IR.BlockStmt (IR.Block [DeclareStmt [IR.Param (ElementaryType Int) (IR.Identifier "x")] [LiteralExpr (IntLiteral {isHex = False, intVal = 3})], ExprStmt (UnaryExpr IR.PostIncrement (IdentifierExpr (IR.Identifier "x"))), AssignStmt [IR.IdentifierExpr (IR.Identifier "x")] [LiteralExpr (IntLiteral {isHex = False, intVal = 1})]]))

        itstmt
          "empty"
          "{ }"
          (IR.BlockStmt (IR.Block []))

      describe "#EmitStatement" $ do

        itstmt'
          "EmitStatement"
          "emit Log(msg.sender, \"Hello World!\");"
           
        itstmt'
          "EmitStatement"
          "emit AnotherLog();"

        itstmt'
          "EmitStatement"
          "emit Sent(msg.sender, receiver, amount);"

      describe "#IfStatement" $ do

        itstmt
          "IfStatement"
          "if(true) {true;}"
          (IfStmt {cond = LiteralExpr (BoolLiteral True), trueBranch = BlockStmt (IR.Block [ExprStmt (LiteralExpr (BoolLiteral True))]), falseBranch = Nothing})


        itstmt
          "IfStatement"
          "if(true) {true;} else { false;}"
          (IfStmt {cond = LiteralExpr (BoolLiteral True), trueBranch = BlockStmt (IR.Block [ExprStmt (LiteralExpr (BoolLiteral True))]), falseBranch = Just (BlockStmt (IR.Block [ExprStmt (LiteralExpr (BoolLiteral False))]))})


      describe "#RequireStatement" $ do
        itstmt
          "RequireStatement"
          "require(true);"
          (RequireStmt {verifyExpr = LiteralExpr (BoolLiteral True)})

        itstmt
          "RequireStatement with message"
          "require(true, \"a message\");"
          (RequireStmt {verifyExpr = LiteralExpr (BoolLiteral True)})

        itstmt
          "RequireStatement with message"
          "require(a == b, \"a message\");"
          (RequireStmt {verifyExpr = BinaryExpr {binaryOp = IR.Equal, lExpr = IdentifierExpr (IR.Identifier "a"), rExpr = IdentifierExpr (IR.Identifier "b")}})


