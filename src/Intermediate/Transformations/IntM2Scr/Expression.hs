{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Intermediate.Transformations.IntM2Scr.Expression where

import Intermediate.Transformations.Base
import Intermediate.Spec as IntM
import Scrypt.Spec as Scr

instance ToScryptTransformable IExpr' (Maybe (Scr.Expr IExpr)) where
  _toScrypt (Just e@(LiteralExpr (IntM.BoolLiteral b))) = Just $ Scr.BoolLiteral b e
  _toScrypt (Just e@(LiteralExpr (IntM.IntLiteral _isHex i))) = Just $ Scr.IntLiteral _isHex i e
  _toScrypt e = error $ "_toScrypt for `" ++ show e ++ "` not implemented in scrypt"
