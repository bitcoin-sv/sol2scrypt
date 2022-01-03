{-# LANGUAGE FlexibleContexts #-}
module Generables.Type where

import Scrypt.Generables.Base
import Scrypt.Generables.Type ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance Generable Scr.Type" $ do
  let itcode title e c = it ("should generate sCrypt code for `" ++ title ++ "` correctly") $ do
        e' <- generateScrypt (CodeGenState 0) (Just e)
        e' `shouldBe` c

  describe "#Type" $ do

    itcode "Bool" Scr.Bool "bool"

    itcode "Int" Scr.Int "int"

    itcode "Bytes" Scr.Bytes "bytes"

    itcode "Any" Scr.Any "auto"

    itcode "Address" (Scr.SubBytes Scr.Ripemd160) "Ripemd160"
