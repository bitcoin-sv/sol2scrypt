{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Expr where

import Numeric
import Scrypt.Generables.Base
import Scrypt.Spec as Scr
import Utils (showHexPadded, showHexWithPadded)

instance Generable (Maybe (Scr.Expr a)) where
  genCode (Just (Scr.BoolLiteral b _)) = if b then "true" else "false"
  genCode (Just (Scr.IntLiteral _isHex i _)) = if _isHex then showHex_ else showInt_
    where
      showHex_ = if i < 0 then "-(" ++  "0x" ++ showHex (negate i) "" ++ ")" else "0x" ++ showHex i ""
      showInt_ = if i < 0 then '-' : showInt (negate i) "" else showInt i ""
  genCode (Just (Scr.BytesLiteral b _)) = "b'" ++ concatMap showHexWithPadded b ++ "'"
  genCode _ = error "unimplemented show scrypt expr"
