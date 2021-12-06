{-# OPTIONS_GHC -fno-warn-orphans #-}

module Scrypt.Generables.Expr where

import Numeric
import Scrypt.Generables.Base
import Scrypt.Spec as Scr

instance Generable (Scr.Expr a) where
  genCode (Scr.BoolLiteral b _) = if b then "true" else "false"
  genCode (Scr.IntLiteral _isHex i _) = if _isHex then "0x" ++ hexWithPadding else showInt i ""
    where
      hex = showHex i ""
      hexWithPadding = if even (length hex) then hex else "0" ++ hex
  genCode _ = error "unimplemented show scrypt expr"
