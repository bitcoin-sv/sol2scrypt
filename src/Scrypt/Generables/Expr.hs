{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Expr where

import Numeric
import Scrypt.Generables.Base
import Scrypt.Spec as Scr
import Utils (showHexWithPadded)

instance Generable (Maybe (Scr.Expr a)) where
  genCode (Just (Scr.BoolLiteral b _)) = if b then "true" else "false"
  genCode (Just (Scr.IntLiteral _isHex i _)) = if _isHex then showHex_ else showInt_
    where
      showHex_ = "0x" ++ showHex i ""
      showInt_ = showInt i ""
  genCode (Just (Scr.BytesLiteral b _)) = "b'" ++ concatMap showHexWithPadded b ++ "'"
  genCode (Just (Scr.UnaryExpr Scr.Negate (Scr.IntLiteral _isHex i _) _)) = "-" ++ if _isHex then showHex_ else showInt_
    where
      showHex_ = "(0x" ++ showHex i "" ++ ")"
      showInt_ = showInt i ""
  genCode _ = error "unimplemented show scrypt expr"
