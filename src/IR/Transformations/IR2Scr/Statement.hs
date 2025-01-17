{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Statement where

import IR.Transformations.Base
import IR.Spec as IR
import IR.Transformations.IR2Scr.Expression ()
import IR.Transformations.IR2Scr.Variable ()
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IStatement' (Maybe (Scr.Statement Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable [IStatement'] [Maybe (Scr.Statement Ann)] where
  _toScrypt = map _toScrypt

instance ToScryptTransformable IStatement (Scr.Statement Ann) where
  _toScrypt (IR.ExprStmt e) = Scr.ExprStmt (_toScrypt e) nil
  _toScrypt (IR.AssignStmt [lhs] [e]) = 
    let lhs' = _toScrypt lhs
        e' = _toScrypt e
      in Scr.Assign lhs' e' nil
  -- declare only allows one declared identifier and expr on the left & right, respectively
  _toScrypt (IR.DeclareStmt [declare] [e]) =
    let declare' :: Scr.Param Ann = _toScrypt declare
        e' = _toScrypt e
    in Scr.Declare declare' e' nil
  _toScrypt (IR.ReturnStmt e) = Scr.ReturnStmt (_toScrypt e) nil
  _toScrypt (IR.RequireStmt e) = Scr.Require (_toScrypt e) nil
  _toScrypt (IR.BlockStmt (IR.Block stmts)) = Scr.Block (map _toScrypt stmts) nil
  _toScrypt (IR.IfStmt e ifstmt elsestmt) =
    let e'  = _toScrypt e
        ifstmt' = _toScrypt ifstmt
        elsestmt' = _toScrypt elsestmt
    in Scr.If e' ifstmt' elsestmt' nil
  _toScrypt (IR.ExitStmt e) = Scr.Exit (_toScrypt e) nil
  _toScrypt (IR.LoopStmt c var body) = Scr.Loop (_toScrypt c) (_toScrypt var) (_toScrypt body) nil
  _toScrypt e = error $ "_toScrypt for `" ++ headWord (show e) ++ "` not implemented in scrypt"
