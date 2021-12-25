{-# LANGUAGE FlexibleContexts #-}
module Generables.Function where

import Scrypt.Generables.Base
import Scrypt.Generables.Function ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "instance Generable (Scr.Function Ann)" $ do
  let itGenCode title e c = it ("should generate sCrypt code for `" ++ title ++ "` correctly") $ do
        genCode e `shouldBe` c

  itGenCode "public function"
    (pubFunc1 Public) 
    "public function add(int x, int y) { require(true); }"

  itGenCode "private function"
    (nonPubFunc1 Private) 
    "private function add(int x, int y) : bool { return true; }"

  itGenCode "function"
    (nonPubFunc1 Default) 
    "function add(int x, int y) : bool { return true; }"
  
  where
    nonPubFunc1 vis =
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

    pubFunc1 vis =
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

    
