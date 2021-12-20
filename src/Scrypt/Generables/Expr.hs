{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Expr where

import Numeric
import Scrypt.Generables.Base
import Scrypt.Spec as Scr
import Utils (showHexWithPadded)

instance Generable (Maybe (Scr.Expr a)) where
  genCode Nothing = ""
  genCode (Just e) = genCodeExpr e

genCodeExpr :: Scr.Expr a -> String
genCodeExpr (Scr.BoolLiteral b _) = if b then "true" else "false"
genCodeExpr (Scr.IntLiteral _isHex i _) = if _isHex then showHex_ else showInt_
    where
      showHex_ = "0x" ++ showHex i ""
      showInt_ = showInt i ""
genCodeExpr (Scr.BytesLiteral b _) = "b'" ++ concatMap showHexWithPadded b ++ "'"
genCodeExpr (Scr.UnaryExpr Scr.Negate (Scr.IntLiteral _isHex i _) _) = "-" ++ if _isHex then showHex_ else showInt_
  where
    showHex_ = "(0x" ++ showHex i "" ++ ")"
    showInt_ = showInt i ""
genCodeExpr _ = error "unimplemented show scrypt expr"
