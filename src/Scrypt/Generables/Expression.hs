{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Expression where

import Data.List
import Numeric
import Scrypt.Generables.Base
import Scrypt.Spec as Scr
import Utils (showHexWithPadded)

instance Generable (Maybe (Scr.Expr a)) where
  genCode = maybe (return "") genCode

instance Generable (Scr.Expr a) where
  genCode (Scr.BoolLiteral b _) = return $ if b then "true" else "false"
  genCode (Scr.IntLiteral _isHex i _) = return $ if _isHex then showHex_ else showInt_
      where
        showHex_ = "0x" ++ showHex i ""
        showInt_ = showInt i ""
  genCode (Scr.BytesLiteral b _) = return $ "b'" ++ concatMap showHexWithPadded b ++ "'"
  -- Var
  genCode (Scr.Var v _ _) = return v
  -- Parens
  genCode (Scr.Parens e _)  = do
    e' <- genCode e
    return $ "(" ++ e' ++ ")"
  -- UnaryExpr
  genCode (Scr.UnaryExpr Scr.Negate e@(Scr.IntLiteral True _ _) _) = do
    e' <- genCode e
    return $ unaryOp2Str Scr.Negate ++ "(" ++ e' ++ ")"
  genCode (Scr.UnaryExpr op e _) | op `elem` [Scr.PostIncrement, Scr.PostDecrement] = do
    e' <- genCode e
    return $ e' ++ unaryOp2Str op
  genCode (Scr.UnaryExpr op e _) = do
    e' <- genCode e
    return $ unaryOp2Str op ++ e'
  -- BinaryExpr
  genCode (Scr.BinaryExpr op e1 e2 _) = do
    e1' <- genCode e1
    e2' <- genCode e2
    return $ e1' ++ binaryOp2Str op ++ e2'
  genCode (Scr.TernaryExpr e1 e2 e3 _) = do
    e1' <- genCode e1
    e2' <- genCode e2
    e3' <- genCode e3
    return $ e1' ++ " ? " ++ e2' ++ " : " ++ e3'
  genCode (Scr.Call n ps _) = do
    ps' <- mapM genCode ps
    return $  n ++ "(" ++ intercalate ", " ps' ++ ")"
  genCode (Scr.Dispatch fn _ mn ps _) = do
    fn' <- genCode fn
    ps' <- mapM genCode ps
    return $ fn' ++ "." ++ mn ++ "(" ++ intercalate ", " ps' ++ ")"
  -- ArrayLiteral
  genCode (Scr.ArrayLiteral array _) = do
    array' <- mapM genCode array
    return $ "[" ++ intercalate ", " array' ++ "]"
  genCode _ = error "unimplemented show scrypt expr"

unaryOp2Str :: Scr.UnaryOp -> String
unaryOp2Str Scr.Not = "!"
unaryOp2Str Scr.Negate = "-"
unaryOp2Str Scr.PostIncrement = "++"
unaryOp2Str Scr.PreIncrement = "++"
unaryOp2Str Scr.PreDecrement = "--"
unaryOp2Str Scr.PostDecrement = "--"
unaryOp2Str op = error $ "unimplemented genCode for unary op `" ++ show op ++ "`"

binaryOp2Str :: Scr.BinaryOp -> String
binaryOp2Str Scr.Add = " + "
binaryOp2Str Scr.Sub = " - "
binaryOp2Str Scr.Mul = " * "
binaryOp2Str Scr.Div = " / "
binaryOp2Str Scr.Mod = " % "
binaryOp2Str Scr.AddAssign = " += "
binaryOp2Str Scr.SubAssign = " -= "
binaryOp2Str Scr.MulAssign = " *= "
binaryOp2Str Scr.DivAssign = " /= "
binaryOp2Str Scr.ModAssign = " %= "
binaryOp2Str Scr.Equal = " == "
binaryOp2Str Scr.Neq = " != "
binaryOp2Str Scr.LessThan = " < "
binaryOp2Str Scr.LessThanOrEqual = " <= "
binaryOp2Str Scr.GreaterThan = " > "
binaryOp2Str Scr.GreaterThanOrEqual = " >= "
binaryOp2Str Scr.BoolAnd = " && "
binaryOp2Str Scr.BoolOr = " || "
binaryOp2Str Scr.Dot = "."
binaryOp2Str op =  error $ "unimplemented genCode for unary op `" ++ show op ++ "`"