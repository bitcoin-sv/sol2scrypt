{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IR.Transformations.IR2Scr.Expression where

import IR.Transformations.Base
import IR.Transformations.IR2Scr.Identifier ()
import IR.Spec as IR
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IExpr' (Maybe (Scr.Expr Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IExpr (Scr.Expr Ann) where
  _toScrypt (LiteralExpr (IR.BoolLiteral b)) = Scr.BoolLiteral b nil
  _toScrypt (LiteralExpr (IR.IntLiteral _isHex i)) = Scr.IntLiteral _isHex i nil
  _toScrypt (LiteralExpr (IR.BytesLiteral b)) = Scr.BytesLiteral b nil
  _toScrypt (IdentifierExpr i) = let (NameAnn n a) :: NameAnn Ann = _toScrypt i in Scr.Var n False a
  _toScrypt (IR.Parens ie) = Scr.Parens (_toScrypt ie) nil
  _toScrypt (IR.UnaryExpr op ie) = Scr.UnaryExpr (toScryptUnaryOp op) (_toScrypt ie) nil
  _toScrypt (IR.BinaryExpr op ie1 ie2) = Scr.BinaryExpr (toScryptBinaryOp op) (_toScrypt ie1) (_toScrypt ie2) nil
  _toScrypt e = error $ "IExpr `" ++ show e ++ "` not implemented in scrypt"

toScryptUnaryOp :: IR.IUnaryOp -> Scr.UnaryOp
toScryptUnaryOp op = read $ show op

toScryptBinaryOp :: IR.IBinaryOp -> Scr.BinaryOp
toScryptBinaryOp op = read $ show op