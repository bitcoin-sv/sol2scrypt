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
  _toScrypt = (<$>) toScryptExpr

toScryptExpr :: IExpr -> Scr.Expr Ann
toScryptExpr (LiteralExpr (IR.BoolLiteral b)) = Scr.BoolLiteral b nil
toScryptExpr (LiteralExpr (IR.IntLiteral _isHex i)) = Scr.IntLiteral _isHex i nil
toScryptExpr (LiteralExpr (IR.BytesLiteral b)) = Scr.BytesLiteral b nil
toScryptExpr (IdentifierExpr i) = let (NameAnn n a) :: NameAnn Ann = _toScrypt i in Scr.Var n False a
toScryptExpr (IR.Parens ie) = Scr.Parens (toScryptExpr ie) nil
toScryptExpr (IR.UnaryExpr op ie) = Scr.UnaryExpr (toScryptUnaryOp op) (toScryptExpr ie) nil
toScryptExpr (IR.BinaryExpr op ie1 ie2) = Scr.BinaryExpr (toScryptBinaryOp op) (toScryptExpr ie1) (toScryptExpr ie2) nil
toScryptExpr e = error $ "IExpr `" ++ show e ++ "` not implemented in scrypt"

toScryptUnaryOp :: IR.IUnaryOp -> Scr.UnaryOp
toScryptUnaryOp IR.Negate = Scr.Negate
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