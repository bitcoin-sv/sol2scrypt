{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Expr where

import Numeric
import Scrypt.Generables.Base
import Scrypt.Spec as Scr
import Utils (showHexWithPadded)

instance Generable (Maybe (Scr.Expr a)) where
  genCode = maybe "" genCode

instance Generable (Scr.Expr a) where
  genCode (Scr.BoolLiteral b _) = if b then "true" else "false"
  genCode (Scr.IntLiteral _isHex i _) = if _isHex then showHex_ else showInt_
      where
        showHex_ = "0x" ++ showHex i ""
        showInt_ = showInt i ""
  genCode (Scr.BytesLiteral b _) = "b'" ++ concatMap showHexWithPadded b ++ "'"
  -- Var
  genCode (Scr.Var v _ _) = v
  -- Parens
  genCode (Scr.Parens e _)  = "(" ++ genCode e ++ ")"
  -- UnaryExpr
  genCode (Scr.UnaryExpr Scr.Negate e@(Scr.IntLiteral True _ _) _) = unaryOp2Str Scr.Negate ++ "(" ++ genCode e ++ ")"
  genCode (Scr.UnaryExpr op e _) | op `elem` [Scr.PostIncrement, Scr.PostDecrement] = genCode e ++ unaryOp2Str op
  genCode (Scr.UnaryExpr op e _) = unaryOp2Str op ++ genCode e 
  -- BinaryExpr
  genCode (Scr.BinaryExpr op e1 e2 _) = genCode e1 ++ binaryOp2Str op ++ genCode e2

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
binaryOp2Str op =  error $ "unimplemented genCode for unary op `" ++ show op ++ "`"