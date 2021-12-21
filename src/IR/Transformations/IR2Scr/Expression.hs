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
toScryptUnaryOp IR.Negate = Scr.Negate
toScryptUnaryOp IR.Not = Scr.Not 
toScryptUnaryOp IR.PostIncrement = Scr.PostIncrement
toScryptUnaryOp IR.PreIncrement = Scr.PreIncrement
toScryptUnaryOp IR.PreDecrement = Scr.PreDecrement
toScryptUnaryOp IR.PostDecrement = Scr.PostDecrement
toScryptUnaryOp op = error $ "unimplemented tranform from IR op `" ++ show op ++ "` to scrypt"

toScryptBinaryOp :: IR.IBinaryOp -> Scr.BinaryOp
toScryptBinaryOp IR.Add = Scr.Add
toScryptBinaryOp IR.Sub = Scr.Sub
toScryptBinaryOp IR.Mul = Scr.Mul
toScryptBinaryOp IR.Div = Scr.Div
toScryptBinaryOp IR.Mod = Scr.Mod
toScryptBinaryOp IR.AddAssign = Scr.AddAssign
toScryptBinaryOp IR.SubAssign = Scr.SubAssign
toScryptBinaryOp IR.MulAssign = Scr.MulAssign
toScryptBinaryOp IR.DivAssign = Scr.DivAssign
toScryptBinaryOp IR.ModAssign = Scr.ModAssign
toScryptBinaryOp IR.Equal = Scr.Equal
toScryptBinaryOp IR.Neq = Scr.Neq
toScryptBinaryOp IR.LessThan = Scr.LessThan
toScryptBinaryOp IR.LessThanOrEqual = Scr.LessThanOrEqual
toScryptBinaryOp IR.GreaterThan = Scr.GreaterThan
toScryptBinaryOp IR.GreaterThanOrEqual = Scr.GreaterThanOrEqual
toScryptBinaryOp IR.BoolAnd = Scr.BoolAnd
toScryptBinaryOp IR.BoolOr = Scr.BoolOr
toScryptBinaryOp op = error $ "unimplemented tranform from IR op `" ++ show op ++ "` to scrypt"