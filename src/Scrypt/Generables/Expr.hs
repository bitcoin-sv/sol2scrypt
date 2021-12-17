{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Expr where

import Numeric
import Scrypt.Generables.Base
import Scrypt.Spec as Scr

instance Generable (Maybe (Scr.Expr a)) where
  genCode (Just (Scr.BoolLiteral b _)) = if b then "true" else "false"
  genCode (Just (Scr.IntLiteral _isHex i _)) = if _isHex then "0x" ++ int2hex else showInt i ""
    where
      int2hex = showHex i ""
  genCode _ = error "unimplemented show scrypt expr"
