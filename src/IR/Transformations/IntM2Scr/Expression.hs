{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IntM2Scr.Expression where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr

instance ToScryptTransformable IExpr' (Maybe (Scr.Expr IExpr)) where
  _toScrypt (Just e@(LiteralExpr (IR.BoolLiteral b))) = Just $ Scr.BoolLiteral b e
  _toScrypt (Just e@(LiteralExpr (IR.IntLiteral _isHex i))) = Just $ Scr.IntLiteral _isHex i e
  _toScrypt e = error $ "_toScrypt for `" ++ show e ++ "` not implemented in scrypt"
