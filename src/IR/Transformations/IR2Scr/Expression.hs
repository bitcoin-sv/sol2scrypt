{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Expression where

import IR.Transformations.Base
import IR.Spec as IR
import Scrypt.Spec as Scr

instance ToScryptTransformable IExpr' (Maybe (Scr.Expr IExpr)) where
  _toScrypt Nothing = Nothing
  _toScrypt (Just e) = Just $ toScryptExpr e

toScryptExpr :: IExpr -> Scr.Expr IExpr
toScryptExpr e@(LiteralExpr (IR.BoolLiteral b)) = Scr.BoolLiteral b e
toScryptExpr e@(LiteralExpr (IR.IntLiteral _isHex i)) = Scr.IntLiteral _isHex i e
toScryptExpr e@(LiteralExpr (IR.BytesLiteral b)) = Scr.BytesLiteral b e
toScryptExpr e@(IR.UnaryExpr IR.Negate (LiteralExpr (IR.IntLiteral _isHex i))) = Scr.UnaryExpr Scr.Negate (Scr.IntLiteral _isHex i e) e
toScryptExpr e = error $ "IExpr `" ++ show e ++ "` not implemented in scrypt"
