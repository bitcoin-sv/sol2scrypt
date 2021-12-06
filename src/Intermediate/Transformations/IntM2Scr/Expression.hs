module Intermediate.Transformations.IntM2Scr.Expression where

import Intermediate.Spec as IntM
import Scrypt.Spec as Scr

-----------------  Intermediate to sCrypt  -----------------

transformIntermediateExpr :: IExpr -> Scr.Expr IExpr
transformIntermediateExpr e@(IntM.BoolLiteral b) = Scr.BoolLiteral b e
transformIntermediateExpr e@(IntM.IntLiteral _isHex i) = Scr.IntLiteral _isHex i e
transformIntermediateExpr e = error $ "transformIntermediateExpr for `" ++ show e ++ "` not implemented in scrypt"
