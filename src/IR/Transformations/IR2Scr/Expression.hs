{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Expression where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.IR2Scr.Identifier ()
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IExpression' (Maybe (Scr.Expr Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IExpression (Scr.Expr Ann) where
  _toScrypt (LiteralExpr (IR.BoolLiteral b)) = Scr.BoolLiteral b nil
  _toScrypt (LiteralExpr (IR.IntLiteral _isHex i)) = Scr.IntLiteral _isHex i nil
  _toScrypt (LiteralExpr (IR.BytesLiteral b)) = Scr.BytesLiteral b nil
  _toScrypt (IdentifierExpr i) = let (NameAnn n a) :: NameAnn Ann = _toScrypt i in Scr.Var n False a
  _toScrypt (IR.ParensExpr ie) = Scr.Parens (_toScrypt ie) nil
  _toScrypt (IR.UnaryExpr op ie) = Scr.UnaryExpr (toScryptUnaryOp op) (_toScrypt ie) nil
  _toScrypt (IR.BinaryExpr op ie1 ie2) = Scr.BinaryExpr (toScryptBinaryOp op) (_toScrypt ie1) (_toScrypt ie2) nil
  _toScrypt (IR.TernaryExpr e1 e2 e3) = Scr.TernaryExpr (_toScrypt e1) (_toScrypt e2) (_toScrypt e3) nil
  _toScrypt (IR.MemberAccessExpr ins m) = Scr.BinaryExpr Scr.Dot (_toScrypt ins) (_toScrypt m) nil
  _toScrypt (IR.FunctionCallExpr (IdentifierExpr fn) ps) = let (NameAnn n _) :: NameAnn Ann = _toScrypt fn in Scr.Call n (map _toScrypt ps) nil
  _toScrypt (IR.FunctionCallExpr (MemberAccessExpr ins m) ps) = Scr.Dispatch (_toScrypt ins) [] (_toScrypt m) (map _toScrypt ps) nil
  _toScrypt (IR.ArrayLiteralExpr array) = Scr.ArrayLiteral (map _toScrypt array) nil
  _toScrypt e = error $ "IExpr `" ++ show e ++ "` not implemented in scrypt"

toScryptUnaryOp :: IR.IUnaryOp -> Scr.UnaryOp
toScryptUnaryOp = read . show

toScryptBinaryOp :: IR.IBinaryOp -> Scr.BinaryOp
toScryptBinaryOp = read . show