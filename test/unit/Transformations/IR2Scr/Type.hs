{-# LANGUAGE FlexibleContexts #-}

module Transformations.IR2Scr.Type where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IType Type" $ do
  let itType title e1 e2 = it ("should transfrom IR `" ++ title ++ "` to sCrypt Type correctly") $ do
        r <- transform2Scrypt (Just $ IR.ElementaryType e1)
        r `shouldBe` Just e2

  describe "#ElementaryType" $ do
    itType "Bool" IR.Bool Scr.Bool

    itType "Int" IR.Int Scr.Int

    itType "Bytes" IR.Bytes Scr.Bytes

    itType "Bytes" IR.Any Scr.Any

    itType "Bytes" IR.Address (Scr.SubBytes Scr.Ripemd160)
