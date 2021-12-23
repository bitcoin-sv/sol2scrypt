{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Statement where

import IR.Transformations.Base
import IR.Spec as IR
import IR.Transformations.IR2Scr.Expression ()
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IStatement' (Maybe (Scr.Statement Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IStatement (Scr.Statement Ann) where
  _toScrypt (IR.ExprStmt e) = Scr.ExprStmt (_toScrypt e) nil
  -- assignment only allows one identifier and expr on the left & right, respectively
  _toScrypt (IR.AssignStmt [Just idtf] [e]) =
    let idtf' :: NameAnn Ann = _toScrypt idtf
        lhs = Var (unName idtf') False nil
        e' = _toScrypt e
    in Scr.Assign lhs e' nil
  _toScrypt e = error $ "_toScrypt for `" ++ show e ++ "` not implemented in scrypt"