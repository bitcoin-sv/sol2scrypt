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
toScryptExpr e@(IdentifierExpr (IIdentifier i)) = Scr.Var i False e
toScryptExpr e@(IR.UnaryExpr op ie) = Scr.UnaryExpr (toScryptUnaryOp op) (toScryptExpr ie) e
toScryptExpr e@(IR.BinaryExpr op ie1 ie2) = Scr.BinaryExpr (toScryptBinaryOp op) (toScryptExpr ie1) (toScryptExpr ie2) e
toScryptExpr e = error $ "IExpr `" ++ show e ++ "` not implemented in scrypt"

toScryptUnaryOp :: IR.IUnaryOp -> Scr.UnaryOp
toScryptUnaryOp IR.Negate = Scr.Negate
toScryptUnaryOp op = error $ "unimplemented tranform from IR op `" ++ show op ++ "` to scrypt"

toScryptBinaryOp :: IR.IBinaryOp -> Scr.BinaryOp
toScryptBinaryOp IR.Add = Scr.Add
toScryptBinaryOp op = error $ "unimplemented tranform from IR op `" ++ show op ++ "` to scrypt"