{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Scrypt.Generables.Expr where

import Numeric
import Scrypt.Generables.Base
import Scrypt.Spec as Scr
import Utils (showHexWithPadded)

instance Generable (Maybe (Scr.Expr a)) where
  genCode Nothing = ""
  genCode (Just e) = genCodeExpr e

genCodeExpr :: Scr.Expr a -> String
genCodeExpr (Scr.BoolLiteral b _) = if b then "true" else "false"
genCodeExpr (Scr.IntLiteral _isHex i _) = if _isHex then showHex_ else showInt_
    where
      showHex_ = "0x" ++ showHex i ""
      showInt_ = showInt i ""
genCodeExpr (Scr.BytesLiteral b _) = "b'" ++ concatMap showHexWithPadded b ++ "'"
-- Var
genCodeExpr (Scr.Var v _ _) = map (\c -> if c == '$' then '_' else c) v
-- Parens
genCodeExpr (Scr.Parens e _)  = "(" ++ genCodeExpr e ++ ")"
-- UnaryExpr
genCodeExpr (Scr.UnaryExpr Scr.Negate e _) = unaryOp2Str Scr.Negate  ++ genCodeExpr e
genCodeExpr (Scr.UnaryExpr op e _) | op `notElem` [Scr.PostIncrement, Scr.PostDecrement] = unaryOp2Str op ++ genCodeExpr e
genCodeExpr (Scr.UnaryExpr op e _) | op `elem` [Scr.PostIncrement, Scr.PostDecrement] = genCodeExpr e ++ unaryOp2Str op
-- BinaryExpr
genCodeExpr (Scr.BinaryExpr op e1 e2 _) | op `elem` [Add, Sub, Mul, Div, Mod] = genCodeExpr e1 ++ binaryOp2Str op ++ genCodeExpr e2
  | op `elem` [AddAssign, SubAssign, MulAssign, DivAssign, ModAssign] = genCodeExpr e1 ++ binaryOp2Str op ++ genCodeExpr e2
  | op `elem` [Equal, Neq, LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, BoolAnd, BoolOr] = genCodeExpr e1 ++ binaryOp2Str op ++ genCodeExpr e2
genCodeExpr _ = error "unimplemented show scrypt expr"

unaryOp2Str :: Scr.UnaryOp -> String 
unaryOp2Str Scr.Not = "!"
unaryOp2Str Scr.Negate = "-"
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