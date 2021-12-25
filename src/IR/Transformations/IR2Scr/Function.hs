{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Function where

import IR.Transformations.Base
import IR.Transformations.IR2Scr.Identifier ()
import IR.Transformations.IR2Scr.Type ()
import IR.Transformations.IR2Scr.Variable ()
import IR.Transformations.IR2Scr.Statement ()
import IR.Spec as IR
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IFunction'  (Maybe (Scr.Function Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IFunction (Scr.Function Ann) where
  _toScrypt (IR.Function fid (ParamList ps) (IR.Block stmts) rt vis) = funcRetGuard $
    Scr.Function
      (_toScrypt fid)
      (map _toScrypt ps)
      (TypeAnn (_toScrypt rt) nil )
      (RegularBody (map _toScrypt stmts) (Scr.BoolLiteral True nil) nil)
      (_toScrypt vis)
      (Scr.Stc False)
      nil

instance ToScryptTransformable IVisibility Scr.Visibility where
  _toScrypt = read . show

-- check whether the last statement (require or return) is consistant with visibility (public or non-public)
funcRetGuard :: Scr.Function Ann -> Scr.Function Ann
funcRetGuard f@(Scr.Function _ _ _ (RegularBody stmts _ _) vis _ _) =
  case reverse stmts of
    ((Scr.Require _ _) : _) | vis == Scr.Public -> f
    ((Scr.ReturnStmt _ _) : _) | vis == Scr.Private || vis == Scr.Default -> f
    s -> error $ "function `" ++  unName (Scr.funcName f) ++ "` have inconsistant visibility `" ++ show vis ++ "` with the last statement `" ++ show s
funcRetGuard _ = error "to scrypt asm function body not implemented yet"