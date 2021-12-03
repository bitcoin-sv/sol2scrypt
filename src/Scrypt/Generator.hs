module Scrypt.Generator where

import Numeric

import Intermediate
import Scrypt.Spec as Scr

-- from sCrypt Ast to code
class Generator a where
  genCode :: a -> String

generateScryptFromIType :: IType' -> IO String
generateScryptFromIType (Just t) = return $ show $ transformIntermediateType t
generateScryptFromIType _ = return ""


generateScryptFromIExpr :: IExpr' -> IO String
generateScryptFromIExpr (Just e) = return $ genCode $ transformIntermediateExpr e
generateScryptFromIExpr _ = return ""

instance Generator (Scr.Expr a) where
  genCode (Scr.BoolLiteral b _) = if b then "true" else "false"
  genCode (Scr.IntLiteral _isHex i _) = if _isHex then "0x" ++ hexWithPadding else showInt i ""
                                    where 
                                      hex = showHex i ""
                                      hexWithPadding = if even (length hex) then hex else "0" ++ hex
  genCode _ = error "unimplemented show scrypt expr"
